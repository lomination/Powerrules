package lomination.powerrules.parser

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.*
import lomination.powerrules.*

object RuleFileParser extends RegexParsers {
  override def skipWhitespace: Boolean     = false
  override protected val whiteSpace: Regex = "".r // probably not necessary

  val logger = org.log4s.getLogger

  def apply(input: String): Try[RuleFile] =
    parseAll(rules, input) match
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

  type P[T] = Parser[T]

  lazy val wsNl: P[Unit]   = "[\n ]*\n".r ^^^ ()
  def ind(n: Int): P[Unit] = /*r(msgIndent(n))*/ (List.fill(n)("  ").mkString.r ^^^ ())

  // general
  lazy val rules: P[RuleFile]                                      = wsNl.? ~> (tmpTile <~ wsNl).? ~ rep1sep(rule, wsNl) <~ "[\n ]*".r ^^ { case d ~ r => RuleFile(d.getOrElse(TmpTile(255, Dir.m3)), r) }
  lazy val tmpTile: P[TmpTile]                                     = ":" ~>! (numericId ~ dir.?) ^^ { case i ~ d => TmpTile(i, d.getOrElse(Dir.p0)) }
  lazy val rule: P[Rule]                                           = ((ruleName <~ wsNl) /* | errRuleName */ ) ~! repsep(command, wsNl) ^^ { case n ~ c => Rule(n, c) }
  lazy val ruleName: P[String]                                     = "\\[[^\n]+\\]".r ^^ { case s => s.drop(1).dropRight(1) }
  lazy val command: P[Command]                                     = replace | shadow | shape | comment | errCommand
  def cmd(name: P[?])(statements: P[Statement]): P[Seq[Statement]] = name ~>! (((wsNl ~ ind(1)) | " +".r) ~> (statements | errStm)).+ <~ (wsNl | "[\n ]*$".r)
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
  // 'with' statement must be tried after 'withexternal' and 'withinternal'
  // else they will be both considered as 'with' and 'external' or 'internal'
  // will grenerate a parsing error
  lazy val shadow: P[Shadow] = cmd("shadow" | "sd")(withExtStm | withIntStm | withStm | ifStm | modeStm) >> { (seq: Seq[Statement]) =>
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
  lazy val comment: P[Comment] = "#[^\n]*".r <~ (wsNl | "[\n ]*$".r) ^^ { case str => Comment(str) }

  // statements
  def stm[T](name: P[String])(content: P[T])                            = name ~! ((wsNl ~ ind(2)) | " +".r) ~>! content
  def stmRep[T](name: P[String])(repeted: P[T], sep1: P[?], sep2: P[?]) = name ~! ((wsNl ~ ind(2)) | " +".r) ~>! rep1sep(repeted, ((sep2 ~ wsNl ~ ind(2)) | sep1))

  lazy val withStm: P[WithStm]            = stmRep("with")(tile, " +".r, "") ^^ { WithStm(_) }
  lazy val withExtStm: P[WithExternalStm] = stmRep("withexternal")(tile, " +".r, "") ^^ { WithExternalStm(_) }
  lazy val withIntStm: P[WithInternalStm] = stmRep("withinternal")(tile, " +".r, "") ^^ { WithInternalStm(_) }
  lazy val ifStm: P[IfStm]                = stmRep("if" | "when")(cond, " +& +".r, " +& *".r) ^^ { IfStm(_) }
  lazy val randomStm: P[RandomStm]        = stm("random")(random) ^^ { RandomStm(_) }
  lazy val rotateStm: P[RotateStm]        = stmRep("rotate")(dir, " *".r, "") ^^ { RotateStm(_) }
  lazy val modeStm: P[ModeStm]            = stm("mode")(mode) ^^ { ModeStm(_) }
  lazy val applyStm: P[ApplyStm]          = stm("apply")(charGrid) ^^ { ApplyStm(_) }
  lazy val onStm: P[OnStm]                = stm("on")(charGrid) ^^ { OnStm(_) }
  lazy val usingStm: P[UsingStm]          = stmRep("using")(mapLine, "\n" ~ ind(2), "") ^^ { (m: Seq[(Char, Tile | GenericMatcher)]) => UsingStm(m.toMap) }
  lazy val neutralStm: P[NeutralStm]      = stm("neutral")(tile) ^^ { NeutralStm(_) }

  // others
  lazy val cond: P[Cond]                    = (pos <~! " +".r) ~! matcher ^^ { case p ~ m => Cond(p, m) }
  lazy val pos: P[Pos]                      = coords | cardPts // | posErr
  lazy val coords: P[Pos]                   = ("-?\\d+".r <~! " +".r) ~! "-?\\d+".r ^^ { case x ~ y => Pos(x.toInt, y.toInt) }
  lazy val cardPts: P[Pos]                  = ("[nsew]*|o".r ^^ { (str: String) => str.toSeq }) >> { s => if (s.contains('n') && s.contains('s') || s.contains('w') && s.contains('e')) err(msgCardPts) else success(Pos(s.count(_ == 'e') - s.count(_ == 'w'), s.count(_ == 's') - s.count(_ == 'n'))) }
  lazy val matcher: P[Matcher]              = fullM | notEdgeM | genericM | errEdgeM
  lazy val fullM: P[FullMatcher]            = (op <~ " +".r) ~ ("full" | "empty") ^^ { case o ~ w => FullMatcher(if (w == "full") o else o.not) }
  lazy val notEdgeM: P[NotEdgeMatcher.type] = "isnot +edge".r ^^^ NotEdgeMatcher
  lazy val genericM: P[GenericMatcher]      = (op <~ " +".r) ~ rep1sep(tileM, " *\\| *".r) ^^ { case op ~ tms => GenericMatcher(op, tms*) }
  lazy val op: P[Op]                        = r(msgOp)("isnot|is".r ^^ { o => if (o == "is") Op.Is else Op.Isnot })
  lazy val tileM: P[TileMatcher]            = r(msgTmId)(numericId | outsideId) ~! r(msgTmDir)(dir | anyDir).? ^^ { case id ~ dir => TileMatcher(id, dir.getOrElse(AnyDir)) }
  lazy val anyDir: P[AnyDir.type]           = "*" ^^^ AnyDir
  lazy val tile: P[Tile]                    = r(msgId)(numericId) ~! r(msgDir)(dir.?) ^^ { case i ~ d => Tile(i, d.getOrElse(Dir.p0)) }
  lazy val numericId: P[Int]                = "[a-f0-9]{1,2}".r ^^ { Integer.parseInt(_, 16) }
  lazy val outsideId: P[Int]                = ("outside" | "-1") ^^^ -1
  lazy val dir: P[Dir]                      = "[+-]".r ~ "[0-3]".r ^^ { case s ~ t => Dir(if (s == "+") Sign.+ else Sign.-, Times.fromOrdinal(t.toInt)) }
  lazy val random: P[Random]                = "\\d+(?:\\.\\d+)?".r ~ "%?".r ^^ { case n ~ p => if (p == "%") Random(n.toFloat) else Random(n.toFloat * 100) }
  lazy val charGrid: P[Grid[Char]]          = rep1sep(charLine, wsNl ~ ind(2)) ^^ { Grid(_) }
  lazy val charLine: P[Seq[Char]]           = rep1sep(char, " *".r)
  lazy val char: P[Char]                    = "\\S".r ^^ { _.charAt(0) }
  lazy val mapLine: P[(Char, Tile | GenericMatcher)] = (char <~ " *-> *".r) ~ (tile | genericM) ^^ { case c ~ t => (c, t) }
  lazy val mode: P[Boolean]                          = "soft|normal".r ^^ { case m => m == "soft" }

  // errors
  def editErr[T](edit: String => String)(parser: P[T]): P[T] = new P[T] {
    def apply(in: Input): ParseResult[T] = parser(in) match
      case Error(msg, next)   => Failure(edit(msg), next)
      case Failure(msg, next) => Failure(edit(msg), next)
      case other              => other
  }
  def r[T](msg: String)(parser: P[T]): P[T] = editErr(str => s"$msg\n\nDetails:\n$str")(parser)
  def wiki(pages: String*): String          = pages.map((p: String) => s"https://github.com/lomination/Powerrules/wiki/${p}").mkString("\nFor more information, see the wiki:\n", ",\n", "") // github wiki reference
  lazy val errStm: P[Nothing]               = guard("\\S".r) - ("with|withexternal|withinternal|if|when|random|rotate|apply|on|using|neutral") >> (_ => err("The given statement is invalid. Make sure the indentation is correct and the name of the statement is valid." + wiki("Command")))
  lazy val errCommand: P[Nothing]           = err("Command not found." + wiki("Command"))
  lazy val errEdgeM: P[Nothing]             = "is +edge".r >> (_ => err("The edge matcher cannot be positive due to language restriction. Please do not use 'is edge'" + wiki("Condition#edge-matcher")))
  // def msgIndent(n: Int): String             = s"Wrong indentation. Expected $n ident(s) = ${2 * n} spaces." + wiki("Command")
  // lazy val msgPos: String                   = "The given postition is not valid. Use 'x y' where x and y are signed intergers." + wiki("Condition#Position")
  lazy val msgCardPts       = "The given position with cardinal points is not valid because opposite cardinal points were found. Do not use 'n' and 's' at the same time, and likewise for 'w' and 'e'." + wiki("Condition#Position")
  lazy val msgOp: String    = "The given operator is not valid. Expected 'is' or 'isnot'." + wiki("Condition#Operator")
  lazy val msgTmId: String  = "The given tile matcher index is not valid. Expected a numeric hexadecimal index value between '0' and 'ff', or '-1' or the 'outside' keyword." + wiki("Tile#index", "Tile#outside", "Tile#tile-matcher") // ensure it is the right page
  lazy val msgTmDir: String = "The given direction is not valid. Expected whether a sign followed by a digit from '0' to '3' or the wildcard '*'." + wiki("Tile#direction", "Tile#any-direction", "Tile#tile-matcher")                  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  lazy val msgId: String    = "The given tile index is not valid. Expected hexadecimal value between '0' and 'ff'." + wiki("Tile#index")
  lazy val msgDir: String   = "The given tile direction is not valid. Expected a sign followed by a digit from '0' to '3'." + wiki("Tile#direction")
}
