package lomination.powerrules.parser

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.*
import lomination.powerrules.*

object RuleFileParser extends RegexParsers {
  override def skipWhitespace: Boolean     = false
  override protected val whiteSpace: Regex = "".r // probably not necessary

  val logger = org.log4s.getLogger

  type P[T] = Parser[T]

  /** Parse all the input as a Powerrules rule file
    *
    * @param input
    *   The parsed input
    * @return
    *   The parsed rule file wrapped in a `Some` on success. On failure or error, returns `None`.
    */
  def apply(input: String): Try[RuleFile] =
    parseAll(ruleFile, input) match
      case Success(result, next) =>
        logger.info("Parser succeed")
        scala.util.Success(result)
      case Error(msg, next) =>
        val exception = ParsingError(s"Error: fail to parse rule file (at l:${next.pos.line}, c:${next.pos.column}).\n\n" + msg)
        logger.warn(exception)("Parser generated a handled error (parse result = error)")
        scala.util.Failure(exception)
      case Failure(msg, next) =>
        val exception = ParsingError(s"Error: fail to parse rule file (at l:${next.pos.line}, c:${next.pos.column}).\n\n" + msg)
        logger.warn(exception)("Parser generated an unhandled error (parse result = failure)")
        scala.util.Failure(exception)

  // ---------- Methods ---------- //

  extension [T](p: P[T])
    /** Apply edit to the message in case of no success
      *
      * @param parser
      *   The given parser
      * @param edit
      *   Lambda that returns the new message
      * @return
      *   A parser of the same type as `parser` but with `edit(msg)` as message in case of no success
      */
    def editMsg(edit: String => String): P[T] = new P[T] {
      def apply(in: Input): ParseResult[T] = p(in) match
        case Error(msg, next)   => Error(edit(msg), next)
        case Failure(msg, next) => Failure(edit(msg), next)
        case other              => other
    }

    /** Appends the given message at the beginning of the parser message in case of no success
      *
      * @param parser
      *   The given parser
      * @param msg
      *   The new appended message
      * @return
      *   A parser of the same type as `parser` but with `s"$newMsg\n\nDetails:\n$oldMsg"` as message in case of no success
      */
    def editMsg(msg: String): P[T] = p.editMsg(msg + "\n\nDetails:\n" + _)

  /** A parser that parses a given number of indent (tab `\t` or four spaces)
    *
    * @param n
    *   The desired number of ident.
    * @return
    *   `Unit` on success
    */
  def ind(n: Int): P[String] = s"(?:    |\t){$n}".r editMsg Msg.indent(n)

  /** A parser that parses a command
    *
    * @param name
    *   A parser of the name of the parsed command
    * @param statements
    *   A parser that can parser every required and optional statement of the command
    * @return
    *   The sequence of he successfully parsed statements
    */
  def cmd(name: P[?])(statements: P[Statement]): P[Seq[Statement]] = name ~>! rep1((wsNl ~ ind(1) | " +".r) ~> (statements | Err.stm))

  /** A parser that parses a statement
    *
    * @param name
    *   A parser of the name of the parsed statement
    * @param content
    *   The parser of the content of the statement
    * @return
    *   The parsed content
    */
  def stm[T](name: P[String])(content: P[T]): P[T] = name ~>! ((wsNl ~ ind(2)) | " +".r) ~>! content

  /** A parser that parses a statement
    *
    * @param name
    *   A parser of the name of the parsed statement
    * @param repeted
    *   The parser of one element
    * @param sep
    *   The parser of the seperation between elements
    * @return
    *   `stm(name)(rep1sep(repeted, sep))`
    */
  def stm[T](name: P[String])(repeted: P[T])(sep: P[?]): P[Seq[T]] = stm(name)(rep1sep(repeted, sep))

  /** Appends reference to the given pages of the github wiki
    *
    * @param pages
    *   the pages to refer to
    * @return
    *   a string showing a list of links
    */
  def wiki(pages: String*): String = pages.map("https://github.com/lomination/Powerrules/wiki/" + _).mkString("\nFor more information, see the wiki:\n", ",\n", "")

  // ---------- General ---------- //

  /** A parser of any whitespace followed by a newline */
  lazy val wsNl: P[String] = "[\\s]*\n".r

  /** A parser of a global Powerrules file */
  lazy val ruleFile: P[RuleFile] = wsNl.? ~> (tmpTile <~ wsNl).? ~ rep1sep(rule, wsNl) <~ "[\\s]*".r ^^ { case t ~ r => RuleFile(t.getOrElse(TmpTile(255, Dir.m3)), r) }

  /** A parser of a temporary tile */
  lazy val tmpTile: P[TmpTile] = ":" ~>! numericId ~! dir.? ^^ { case i ~ d => TmpTile(i, d.getOrElse(Dir.p0)) }

  /** A parser of a rule */
  lazy val rule: P[Rule] = (ruleName <~ wsNl) ~! repsep(command, wsNl) ^^ { case n ~ c => Rule(n, c) }

  /** A parser of the name of a rule */
  lazy val ruleName: P[String] = "\\[[^\n]+\\]".r ^^ { str => str.drop(1).dropRight(1) }

  // ---------- Commands ---------- //

  /** A parser of a command */
  lazy val command: P[Command] = replace | shadow | shape | comment | Err.command

  /** A parser of a replace command */
  lazy val replace: P[Replace] = cmd("replace" | "re")(withStm | ifStm | randomStm | rotateStm) >> { (seq: Seq[Statement]) =>
    for {
      withStm <- seq.collectFirst { case stm: WithStm => stm }.fold[P[WithStm]](err("Missing 'with' statement in replace command." + wiki("Replace")))(success)
      ifStm     = seq.collectFirst { case stm: IfStm => stm }
      randomStm = seq.collectFirst { case stm: RandomStm => stm }
      rotateStm = seq.collectFirst { case stm: RotateStm => stm }
    } yield Replace(
      withStm.tiles,
      ifStm.getOrElse(IfStm(Seq())).conds,
      randomStm.getOrElse(RandomStm(Random.always)).chance,
      rotateStm.getOrElse(RotateStm(Seq(Dir.p0))).rotations
    )
  }

  /** A parser of a shadow command */
  lazy val shadow: P[Shadow] = cmd("shadow" | "sd")(withExtStm | withIntStm | withStm | ifStm | modeStm) >> { (seq: Seq[Statement]) =>
    // --------------------------------------------/^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\
    // 'with' statement must be tried after 'withexternal' and 'withinternal'
    // else they will be both considered as 'with' and 'external' or 'internal'
    // will grenerate a parsing error
    for {
      withStm <- seq.collectFirst { case stm: WithStm => stm }.fold[P[WithStm]](err("Missing 'with' statement in shadow command." + wiki("Shadow")))(success)
      withExtStm = seq.collectFirst { case stm: WithExternalStm => stm }
      withIntStm = seq.collectFirst { case stm: WithInternalStm => stm }
      ifStm      = seq.collectFirst { case stm: IfStm => stm }
      modeStm    = seq.collectFirst { case stm: ModeStm => stm }
    } yield Shadow(
      withStm.tiles,
      withExtStm.getOrElse(WithExternalStm(Seq())).tiles,
      withIntStm.getOrElse(WithInternalStm(Seq())).tiles,
      ifStm.getOrElse(IfStm(Seq())).conds,
      modeStm.getOrElse(ModeStm(false)).softMode
    )
  }

  /** A parser of a shape command */
  lazy val shape: P[Shape] = cmd("shape" | "sp")(applyStm | onStm | usingStm | neutralStm | randomStm | rotateStm) >> { (seq: Seq[Statement]) =>
    for {
      applyStm   <- seq.collectFirst { case stm: ApplyStm => stm }.fold[P[ApplyStm]](err("Missing 'apply' statement in shape command." + wiki("Shape")))(success)
      onStm      <- seq.collectFirst { case stm: OnStm => stm }.fold[P[OnStm]](err("Missing 'on' statement in shape command." + wiki("Shape")))(success)
      usingStm   <- seq.collectFirst { case stm: UsingStm => stm }.fold[P[UsingStm]](err("Missing 'using' statement in shape command." + wiki("Shape")))(success)
      neutralStm <- seq.collectFirst { case stm: NeutralStm => stm }.fold[P[NeutralStm]](err("Missing 'neutral' statement in shape command." + wiki("Shape")))(success)
      randomStm  <- seq.collectFirst { case stm: RandomStm => stm }.fold[P[RandomStm]](err("Missing 'random' statement in shape command." + wiki("Shape")))(success)
      rotateStm = seq.collectFirst { case stm: RotateStm => stm }
    } yield
      val newMap = usingStm.map ++ Map('!' -> FullMatcher(Op.Isnot), '.' -> FullMatcher(Op.Is)) - '?'
      Shape(
        applyStm.chars.map(c =>
          newMap.get(c) match
            case Some(t: Tile)    => Some(t)
            case Some(m: Matcher) => None
            case None             => None
        ),
        onStm.chars.map(c =>
          newMap.get(c) match
            case Some(t: Tile)    => Some(GenericMatcher(Op.Is, t.toTileMatcher))
            case Some(m: Matcher) => Some(m)
            case None             => None
        ),
        neutralStm.tile,
        randomStm.chance,
        rotateStm.getOrElse(RotateStm(Seq(Dir.p0))).rotations
      )
  }

  // ---------- Statements ---------- //

  /** A parser of a comment */
  lazy val comment: P[Comment] = "#[^\n]*".r <~ (wsNl | "[\\s]*$".r) ^^ { str => Comment(str) }

  /** Parser of with statement */
  lazy val withStm: P[WithStm] = stm("with")(tile)(wsNl ~ ind(2) | " +".r) ^^ { WithStm(_) }

  /** Parser of withexternal statement */
  lazy val withExtStm: P[WithExternalStm] = stm("withexternal")(tile)(wsNl ~ ind(2) | " +".r) ^^ { WithExternalStm(_) }

  /** Parser of withinternal statement */
  lazy val withIntStm: P[WithInternalStm] = stm("withinternal")(tile)(wsNl ~ ind(2) | " +".r) ^^ { WithInternalStm(_) }

  /** Parser of if statement */
  lazy val ifStm: P[IfStm] = stm("if" | "when")(cond)(" *&".r ~ wsNl ~ ind(2) | " +& +".r) ^^ { IfStm(_) }

  /** Parser of random statement */
  lazy val randomStm: P[RandomStm] = stm("random")(random) ^^ { RandomStm(_) }

  /** Parser of rotate statement */
  lazy val rotateStm: P[RotateStm] = stm("rotate")(dir)(" *".r) ^^ { RotateStm(_) }

  /** Parser of mode statement */
  lazy val modeStm: P[ModeStm] = stm("mode")(mode) ^^ { ModeStm(_) }

  /** Parser of apply statement */
  lazy val applyStm: P[ApplyStm] = stm("apply")(charGrid) ^^ { ApplyStm(_) }

  /** Parser of on statement */
  lazy val onStm: P[OnStm] = stm("on")(charGrid) ^^ { OnStm(_) }

  /** Parser of using statement */
  lazy val usingStm: P[UsingStm] = stm("using")(mapLine)(wsNl ~ ind(2)) ^^ { (m: Seq[(Char, Tile | GenericMatcher)]) => UsingStm(m.toMap) }

  /** Parser of neutral statement */
  lazy val neutralStm: P[NeutralStm] = stm("neutral")(tile) ^^ { NeutralStm(_) }

  // ---------- Other objects ---------- //

  lazy val cond: P[Cond]  = (pos <~! " +".r) ~! matcher ^^ { case p ~ m => Cond(p, m) }
  lazy val pos: P[Pos]    = (coords | cardPts | oPos) editMsg (Msg.pos)
  lazy val coords: P[Pos] = guard("[-\\d]".r) ~>! ("-?\\d+".r <~ " +".r) ~ "-?\\d+".r ^^ { case x ~ y => Pos(x.toInt, y.toInt) }
  lazy val cardPts: P[Pos] = guard("[nsew]".r) ~>! ("[nsew]+".r ^^ { (str: String) => str.toSeq }) >> { s =>
    if (s.contains('n') && s.contains('s') || s.contains('w') && s.contains('e')) err(Msg.cardPts) else success(Pos(s.count(_ == 'e') - s.count(_ == 'w'), s.count(_ == 's') - s.count(_ == 'n')))
  }
  lazy val oPos: P[Pos]                              = "o|there".r ^^^ Pos.zero
  lazy val matcher: P[Matcher]                       = fullM | notEdgeM | genericM | Err.edgeM
  lazy val fullM: P[FullMatcher]                     = (op <~ " +".r) ~ ("full" | "empty") ^^ { case o ~ w => FullMatcher(if (w == "full") o else o.not) }
  lazy val notEdgeM: P[NotEdgeMatcher.type]          = "isnot +edge".r ^^^ NotEdgeMatcher
  lazy val genericM: P[GenericMatcher]               = (op <~ " +".r) ~ rep1sep(tileM, " *\\| *".r) ^^ { case op ~ tms => GenericMatcher(op, tms.toSeq*) }
  lazy val op: P[Op]                                 = (commit("isnot|is".r ^^ { o => if (o == "is") Op.Is else Op.Isnot })) editMsg (Msg.op)
  lazy val tileM: P[TileMatcher]                     = (commit(numericId | outsideId) editMsg (Msg.tmId)) ~! (commit(dir | anyDir) editMsg (Msg.tmDir)).? ^^ { case id ~ dir => TileMatcher(id, dir.getOrElse(AnyDir)) }
  lazy val anyDir: P[AnyDir.type]                    = "*" ^^^ AnyDir
  lazy val tile: P[Tile]                             = ( /* commit */ (numericId) editMsg (Msg.id)) ~! (commit(dir.?) editMsg (Msg.dir)) ^^ { case i ~ d => Tile(i, d.getOrElse(Dir.p0)) } // removed commit to fix ParseShadow (5)
  lazy val numericId: P[Int]                         = "[a-f0-9]{1,2}".r ^^ { Integer.parseInt(_, 16) }
  lazy val outsideId: P[Int]                         = ("outside" | "-1") ^^^ -1
  lazy val dir: P[Dir]                               = "[+-]".r ~ "[0-3]".r ^^ { case s ~ t => Dir(if (s == "+") Sign.+ else Sign.-, Times.fromOrdinal(t.toInt)) }
  lazy val random: P[Random]                         = "\\d+(?:\\.\\d+)?".r ~ "%?".r ^^ { case n ~ p => if (p == "%") Random(n.toFloat) else Random(n.toFloat * 100) }
  lazy val charGrid: P[Grid[Char]]                   = rep1sep(charLine, wsNl ~ ind(2)) ^^ { Grid(_) }
  lazy val charLine: P[Seq[Char]]                    = rep1sep(char, " *".r)
  lazy val char: P[Char]                             = "\\S".r ^^ { _.charAt(0) }
  lazy val mapLine: P[(Char, Tile | GenericMatcher)] = (char <~! " *-> *".r) ~! (tile | genericM) ^^ { case c ~ t => (c, t) }
  lazy val mode: P[Boolean]                          = commit("soft|normal".r) ^^ { case m => m == "soft" } editMsg (Msg.mode)

  // ---------- Error handling ---------- //

  /** Object that contains error parsers */
  object Err:
    lazy val stm: P[Nothing]     = guard("\\S".r) - ("with|withexternal|withinternal|if|when|random|rotate|apply|on|using|neutral") >> (_ => err("The given statement is invalid. Make sure the indentation is correct and the name of the statement is valid." + wiki("Commands")))
    lazy val command: P[Nothing] = err("Command not found." + wiki("Commands"))
    lazy val edgeM: P[Nothing]   = "is +edge".r >> (_ => err("The edge matcher cannot be positive due to language restriction. Please do not use 'is edge'" + wiki("Condition#edge-matcher")))

  /** Object that contins error messages */
  object Msg:
    def indent(n: Int): String = s"Wrong indentation. Expected $n ident(s) = ${4 * n} spaces." + wiki("Commands")
    lazy val pos: String       = "The given postition is not valid. Expected either coordinates 'x y' where x and y are signed intergers, or cardinal points ('n', 's', 'e', 'w') or 'o' or 'there'" + wiki("Condition#Position")
    lazy val cardPts           = "The given position with cardinal points is not valid because opposite cardinal points were found. Do not use 'n' and 's' at the same time, and likewise for 'w' and 'e'." + wiki("Condition#Position")
    lazy val op: String        = "The given operator is not valid. Expected 'is' or 'isnot'." + wiki("Condition#Operator")
    lazy val tmId: String      = "The given tile matcher index is not valid. Expected a numeric hexadecimal index value between '0' and 'ff', or '-1' or the 'outside' keyword." + wiki("Tile#index", "Tile#outside", "Tile#tile-matcher")
    lazy val tmDir: String     = "The given direction is not valid. Expected whether a sign followed by a digit from '0' to '3' or the wildcard '*'." + wiki("Tile#direction", "Tile#any-direction", "Tile#tile-matcher")
    lazy val id: String        = "The given tile index is not valid. Expected hexadecimal value between '0' and 'ff'." + wiki("Tile#index")
    lazy val dir: String       = "The given tile direction is not valid. Expected a sign followed by a digit from '0' to '3'." + wiki("Tile#direction")
    lazy val mode: String      = "The given mode is not valid. Expected: either 'normal' or 'soft'." + wiki("Mode")
}
