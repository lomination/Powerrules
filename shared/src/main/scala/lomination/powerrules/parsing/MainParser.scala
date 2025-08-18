package lomination.powerrules.parsing

import lomination.powerrules.ast._
import lomination.powerrules.lexing.tokens.Token
import lomination.powerrules.lexing.{TokenParser, tokens => tks}
import lomination.powerrules.parsing._
import lomination.powerrules.util.style.{ansi0, ansi31, ansi4}

import scala.util.Try

/** Object that parses Powerrules Ast from tokens. */
object MainParser extends TokenParser {

  /** Parse all the input as a Powerrules file ast.
    *
    * @param input
    *   the parsed input.
    * @return
    *   the parsed rule file wrapped in a `scala.util.Try`. Either `Success(value)` on success or `Failure(exception)` on failure.
    */
  def apply(tokens: Seq[Token]): Try[RuleFile] =
    parse(phrase(ruleFile), TokenReader(tokens)) match
      case Success(result, next) =>
        logger.info("Parser succeed")
        scala.util.Success(result)
      case NoSuccess.I(msg, next) =>
        val exception = TokenParsingError(s"${ansi31}Fail to parse rule file at ${next.pos}.$ansi0\n" + msg)
        logger.error(exception)("Parser failed")
        scala.util.Failure(exception)

  // ---------- Parser extensions ---------- //

  extension [A](parser: P[A])

    /** A parser combinator for inclusive alternative composition. Equivalent to an incluvise or parser operator.
      *
      * @param B
      *   the type of the other parser
      * @param other
      *   a parser
      * @return
      *   a parser that parses `this` or `other` or `this` and `other`. The resulting parser has type `Parser[~[Option[A], Option[B]]]` but note that
      *   both option cannot be `None` at the same time. In the latter case, the resulting parser will fail.
      */
    def ||[B](other: P[B]): P[Option[A] ~ Option[B]] =
      def toOpt[C](parser: P[C]): P[Option[C]] =
        parser ^^ { Some(_) }
      (toOpt(parser) ~ toOpt(other))
        | (toOpt(parser) ~ success(None))
        | (success(None) ~ toOpt(other))

    /** A parser combinator for combination of 2 parsers separated by a space token which is ignored.
      *
      * @param B
      *   the type of the other parser
      * @param other
      *   a parser
      * @return
      *   a parser that is equal to `(parser <~ spaceTk) ~ other`
      */
    def ~~[B](other: P[B]): P[A ~ B] =
      (parser <~ spaceTk) ~ other

  // ---------- Parser functions ---------- //

  /** A parser that parses another given parser in an indentation block and returns its result.
    *
    * @param parser
    *   a parser
    * @return
    *   a parser that parses the content of the given parser preceded by an indent token and followed by a dedent token.
    */
  def block[A](parser: P[A]): P[A] =
    indentTk ~> parser <~ dedentTk

  /** A parser that parses another given parser surronded by curly braces and returns its result.
    *
    * @param parser
    *   a parser
    * @return
    *   a parser that parses the content of the given parser preceded by a left curly brace token and followed by a right curly brace token.
    */
  def withAcolades[A](parser: P[A]): P[A] =
    leftAcoladeTk ~> parser <~ rightAcoladeTk

  /** A parser that parses another given parser surronded by optional space tokens. Note that the two space tokens are parsed independantly.
    *
    * @param parser
    *   a parser
    * @return
    *   a parser that parses a space optionally, the given parser, and a space optionally.
    */
  def optSpaced[A](parser: P[A]): P[A] =
    spaceTk.? ~> parser <~ spaceTk.?

  /** A parser that applies another given parser surrounded by parsers of space token.
    *
    * @param parser
    *   a parser
    * @return
    *   a parser that parses a space, the given parser, and another space.
    */
  def spaced[A](parser: P[A]): P[A] =
    spaceTk ~> parser <~ spaceTk

  /** A generic parser that parses a command.
    *
    * @param name
    *   A parser of the name of the parsed command
    * @param statements
    *   A parser that can parser every required and optional statement of the command
    * @return
    *   The sequence of he successfully parsed statements
    */
  def cmd(name: P[Token])(statements: P[Statement]): P[Seq[Statement]] =
    name ~ spaceTk.? ~>! block(rep1sep(statements, newlineTk.?))

  /** An error parser for missing statement when a command is parsed
    *
    * @param stm
    *   the name of the missing statement
    * @param cmd
    *   the name of the command which is being parsed
    * @return
    *   a parser of nothing that, when applied to an input, returns an non back-tracking error.
    */
  def missingStm(stm: String, cmd: String): P[Nothing] =
    err(s"Missing '$stm' statement in $cmd command." + wiki(cmd.capitalize))

  /** Creates a message that refers to the given pages of the github wiki of Powerrules
    *
    * @param pages
    *   the pages to refer to
    * @return
    *   a string showing a list of links
    */
  def wiki(pages: String*): String =
    pages
      .map(ansi4 + "https://github.com/lomination/Powerrules/wiki/" + _ + ansi0)
      .mkString("\nFor more information, see the following wiki pages:\n", ",\n", "")

  // ---------- General ---------- //

  /** A parser of a global Powerrules file */
  lazy val ruleFile: P[RuleFile] =
    newlineTk.? ~>! rep1sep(rule, newlineTk.?) <~ newlineTk.?
      ^^ { rules => RuleFile(rules) }
      |< "powerrules file"

  /** A parser of a rule */
  lazy val rule: P[Rule] =
    (title <~ newlineTk) ~! repsep(command, newlineTk)
      ^^ { case title ~ cmds => Rule(title, cmds) }
      |< "rule"

  /** A parser for a rule's title */
  lazy val title: P[String] =
    leftBracketTk ~> rep(noRightBracket) <~ rightBracketTk
      ^^ { _.map(_.raw).mkString }
      |< "rule title"

  /** A parser that parses any token except the right bracket token */
  lazy val noRightBracket: P[Token] =
    acceptMatch("any token except right bracket character `]`", { case token: Token if !token.isInstanceOf[tks.RightBracket] => token })
      |< "any token except right bracket character `]`"

  /** A parser that parses a command */
  lazy val command: P[Command] =
    replace | shadow | shape | comment |< "command"

  /** A parser that parses a replace command */
  lazy val replace: P[Replace] =
    cmd(replaceTk | reTk)(withStm | ifStm | randomStm)
      >> { (seq: Seq[Statement]) =>
        for {
          withStm <- seq.collectFirst { case stm: WithStm => stm }.fold(missingStm("with", "replace"))(success)
          ifStm     = seq.collectFirst { case stm: IfStm => stm }
          randomStm = seq.collectFirst { case stm: RandomStm => stm }
        } yield Replace(
          withStm.tiles,
          ifStm.map(_.conds).getOrElse(Seq()),
          randomStm.map(_.chance).getOrElse(Random.always)
        )
      }
      |< "replace command"

  /** A parser that parses a shadow command */
  lazy val shadow: P[Shadow] =
    cmd(shadowTk | sdTk)(withStm | withExtStm | withIntStm | ifStm | modeStm)
      >> { (seq: Seq[Statement]) =>
        for {
          withStm <- seq.collectFirst { case stm: WithStm => stm }.fold(missingStm("with", "shadow"))(success)
          withExtStm = seq.collectFirst { case stm: WithExternalStm => stm }
          withIntStm = seq.collectFirst { case stm: WithInternalStm => stm }
          ifStm      = seq.collectFirst { case stm: IfStm => stm }
          modeStm    = seq.collectFirst { case stm: ModeStm => stm }
        } yield Shadow(
          withStm.tiles,
          withExtStm.map(_.tiles).getOrElse(Seq()),
          withIntStm.map(_.tiles).getOrElse(Seq()),
          ifStm.map(_.conds).getOrElse(Seq()),
          modeStm.map(_.softMode).getOrElse(false)
        )
      }
      |< "shadow command"

  /** A parser that parses a shape command */
  lazy val shape: P[Shape] =
    cmd(shapeTk | spTk)(applyStm | onStm | usingStm | randomStm)
      >> { (seq: Seq[Statement]) =>
        for {
          applyStm <- seq.collectFirst { case stm: ApplyStm => stm }.fold(missingStm("apply", "shape"))(success)
          onStm    <- seq.collectFirst { case stm: OnStm => stm }.fold(missingStm("on", "shape"))(success)
          usingStm <- seq.collectFirst { case stm: UsingStm => stm }.fold(missingStm("using", "shape"))(success)
          randomStm = seq.collectFirst { case stm: RandomStm => stm }
        } yield
          val newMap = usingStm.map ++ Map('!' -> (p => Cond(p, Op.Is, EmptyMatcher)), '.' -> (p => Cond(p, Op.Is, FullMatcher))) - '?'
          Shape(
            applyStm.chars.map(c =>
              newMap.get(c) match
                case Some(t: Tile) => Some(t)
                case _             => None
            ),
            onStm.chars.map(c =>
              newMap.get(c) match
                case Some(t: Tile) => Some(Cond(_, Op.Is, GenericMatcher(t.toTileMatcher)))
                case Some(c)       => Some(c.asInstanceOf[(Pos => Cond)])
                case None          => None
            ),
            randomStm.map(_.chance).getOrElse(Random(100))
          )
      }
      |< "shape command"

  /** A parser that parses a comment */
  lazy val comment: P[Comment] =
    hashtagTk ~> rep(noNewline)
      ^^ { toks => Comment(toks.map(_.raw).mkString) }
      |< "comment (command)"

  /** A parser that parses any token except the newline token */
  lazy val noNewline: P[Token] =
    acceptMatch("any token except newline", { case token: Token if !token.isInstanceOf[tks.Newline] => token })
      |< "any token except newline"

  // ---------- Statements ---------- //

  /** Parser of with statement */
  lazy val withStm: P[WithStm] =
    withTk ~>! (spaceTk ~> rep1sep(tile, spaceTk) | block(rep1sep(tile, spaceTk | newlineTk)))
      ^^ { WithStm(_) }
      |< "with statement"

  /** Parser of withexternal statement */
  lazy val withExtStm: P[WithExternalStm] =
    withexternalTk ~>! (spaceTk ~> rep1sep(tile, spaceTk) | block(rep1sep(tile, spaceTk | newlineTk)))
      ^^ { WithExternalStm(_) }
      |< "withexternal statement"

  /** Parser of withinternal statement */
  lazy val withIntStm: P[WithInternalStm] =
    withinternalTk ~>! (spaceTk ~> rep1sep(tile, spaceTk) | block(rep1sep(tile, spaceTk | newlineTk)))
      ^^ { WithInternalStm(_) }
      |< "withinternal statement"

  /** Parser of if statement */
  lazy val ifStm: P[IfStm] =
    ifTk ~>! (spaceTk ~> rep1sep(cond, spaced(ampersandTk)) | block(rep1sep(cond, spaceTk.? ~ ampersandTk ~ newlineTk)))
      ^^ { conds => IfStm(conds.reduce(_ ++ _)) }
      |< "if statement"

  /** Parser of random statement */
  lazy val randomStm: P[RandomStm] =
    randomTk ~>! (spaceTk ~> random | block(random))
      ^^ { RandomStm(_) }
      |< "random statement"

  /** Parser of mode statement */
  lazy val modeStm: P[ModeStm] =
    modeTk ~>! (spaceTk ~> mode | block(mode))
      ^^ { ModeStm(_) }
      |< "mode statement"

  /** Parser of apply statement */
  lazy val applyStm: P[ApplyStm] =
    applyTk ~>! block(charGrid)
      ^^ { ApplyStm(_) }
      |< "apply statement"

  /** Parser of on statement */
  lazy val onStm: P[OnStm] =
    onTk ~>! block(charGrid)
      ^^ { OnStm(_) }
      |< "on statement"

  /** Parser of using statement */
  lazy val usingStm: P[UsingStm] =
    usingTk ~>! block(rep1sep(dictLine, newlineTk))
      ^^ { (m: Seq[(Char, Tile | (Pos => Cond))]) => UsingStm(m.toMap) }
      |< "using statement"

  // ---------- Other objects ---------- //

  /** A parser of condition */
  lazy val cond: P[Seq[Cond]] =
    singleCond ^^ { cond => Seq(cond) } | multiCond
      |< "condition"

  /** A parser of single position condition */
  lazy val singleCond: P[Cond] =
    pos ~~ singularOp ~~ matcher
      >> {
        case pos ~ Op.Is ~ EdgeMatcher => err("A positive matcher is not allowed")
        case pos ~ op ~ matcher        => success(Cond(pos, op, matcher))
      } |< "single position condition"

  /** A parser of multiple positions condition */
  lazy val multiCond: P[Seq[Cond]] =
    (pos <~ optSpaced(commaTk)) ~ rep1sep(pos, optSpaced(commaTk)) ~~ pluralOp ~~ matcher
      >> {
        case first ~ pos ~ Op.Is ~ EdgeMatcher => err("A positive matcher is not allowed")
        case first ~ pos ~ op ~ matcher        => success(first +: pos map (Cond(_, op, matcher)))
      } |< "multi positions condition"

  /** A parser of position */
  lazy val pos: P[Pos] =
    coords | cardPts | there
      |< "position"

  /** A parser of position using coordinates */
  lazy val coords: P[Pos] =
    minusTk.? ~ decimalNumberTk ~~ minusTk.? ~ decimalNumberTk
      ^^ { case xSign ~ x ~ ySign ~ y =>
        def num(sign: Option[?], i: tks.DecimalNumber): Int =
          sign.map(_ => -1).getOrElse(1) * i.content
        Pos(num(xSign, x), num(ySign, y))
      }
      |< "coordinates (position)"

  /** A parser of position using a sequence of cardinal points */
  lazy val cardPts: P[Pos] =
    val isValid = "([nNeE]+|[nNwW]+|[sSeE]+|[sSwW]+)".r
    acceptMatch(
      "sequence of cardinal points (position)",
      { case tks.Literal(isValid(str), _, _, _) =>
        val s = str.toLowerCase
        Pos(
          s.count(_ == 'e') - s.count(_ == 'w'),
          s.count(_ == 's') - s.count(_ == 'n')
        )
      }
    ) |< "sequence of cardinal points (position)"

  /** A parser of position using the `there` keyword */
  lazy val there: P[Pos] =
    thereTk
      ^^^ Pos.zero
      |< "there (position)"

  /** A parser of single position condition operator */
  lazy val singularOp: P[Op] =
    isTk ~> (spaceTk ~> notTk).?
      ^^ { _.map(_ => Op.IsNot).getOrElse(Op.Is) }
      |< "singular condition operator"

  /** A parser of multiple positions condition operator */
  lazy val pluralOp: P[Op] =
    areTk ~> (spaceTk ~> notTk).?
      ^^ { _.map(_ => Op.IsNot).getOrElse(Op.Is) }
      |< "plural condition operator"

  /** A generic parser of a matcher */
  lazy val matcher: P[Matcher] =
    fullMatcher | emptyMatcher | edgeMatcher | genericMatcher
      |< "matcher"

  /** A parser of full matcher denoted by the keyword `full` */
  lazy val fullMatcher: P[FullMatcher.type] =
    fullTk ^^^ FullMatcher
      |< "full matcher"

  /** A parser of empty matcher denoted by the keyword `empty` */
  lazy val emptyMatcher: P[EmptyMatcher.type] =
    emptyTk ^^^ EmptyMatcher
      |< "empty matcher"

  /** A parser of edge matcher denoted by the keyword `edge` */
  lazy val edgeMatcher: P[EdgeMatcher.type] =
    edgeTk ^^^ EdgeMatcher
      |< "edge matcher"

  /** A parser of generic matcher */
  lazy val genericMatcher: P[GenericMatcher] =
    rep1sep(tileMatcher, optSpaced(pipeTk))
      ^^ { case tms => GenericMatcher(tms.toSeq*) }
      |< "generic matcher"

  /** A parder of tile matcher */
  lazy val tileMatcher: P[TileMatcher] =
    (tileIndex | outsideIndex) ~ (dir | anyDir).?
      ^^ { case id ~ dir => TileMatcher(id, dir.getOrElse(AnyDir)) }
      |< "tile matcher"

  /** A parser of a tile's index */
  lazy val tileIndex: P[Int] =
    acceptMatch("tile index in hexadecimal", { case tks.HexaNumber(i, _, _, _) if 0 <= i && i < 256 => i })
      | acceptMatch("tile index in decimal", { case tks.DecimalNumber(i, _, _, _) if 0 <= i && i < 256 => i })
      |< "tile index (decimal or hexadecimal)"

  /** A parser of special tile matcher's index  denoted by `-1` or the `outside` keyword */
  lazy val outsideIndex: P[Int] =
    (outsideTk | minusTk ~ one)
      ^^^ -1
      |< "minus one index"

  /** A parser that parses the digit 1 */
  lazy val one: P[Unit] =
    acceptMatch("one", { case tks.DecimalNumber(1, _, _, _) => () })

  /** A parser of the any direction wildcard `*` */
  lazy val anyDir: P[AnyDir.type] =
    starTk ^^^ AnyDir
      |< "any direction"

  /** A parser of tile */
  lazy val tile: P[Tile] =
    tileIndex ~ dir.?
      ^^ { case id ~ dir => Tile(id, dir.getOrElse(Dir.p0)) }
      |< "tile"

  /** A parser of a tile's direction */
  lazy val dir: P[Dir] =
    ((minusTk | plusTk) ~ decimalNumberTk)
      >> {
        case _ ~ tks.DecimalNumber(int, _, _, _) if int > 3 || int < 0 => failure(s"invalid dir $int")
        case tks.Plus(_, _, _) ~ tks.DecimalNumber(int, _, _, _)       => success(Dir(Sign.+, Times.fromOrdinal(int)))
        case tks.Minus(_, _, _) ~ tks.DecimalNumber(int, _, _, _)      => success(Dir(Sign.-, Times.fromOrdinal(int)))
      }
      |< "direction"

  /** A parser of chance */
  lazy val random: P[Random] =
    float ~ percentTk.?
      ^^ {
        case float ~ Some(_) => Random(float)
        case float ~ _       => Random(float * 100)
      }
      |< "random chance"

  /** A parser of floating point number */
  lazy val float: P[Float] =
    (decimalNumberTk || dotTk ~> decimalNumberTk)
      ^^ { case units ~ decimals =>
        val a = units.map(_.content).getOrElse(0).toFloat
        val b = decimals.map(tok => (tok.content / Math.pow(10, tok.raw.size)).toFloat).getOrElse(0f)
        a + b
      }
      |< "float"

  /** A parser of 2-dimensional grid of char */
  lazy val charGrid: P[Grid[Char]] =
    rep1sep(charLine, newlineTk)
      >> { lines =>
        if (lines.forall(_.sizeCompare(lines.head) == 0))
          success(Grid(lines))
        else
          err("All lines of a pattern (or grid) must have the same length")
      } |< "char pattern"

  /** A parser of single dimensional line of chars */
  lazy val charLine: P[Seq[Char]] =
    rep1sep(char, spaceTk.?)
      |< "line of char pattern"

  /** A parser of non white-space character */
  lazy val char: P[Char] =
    val singleChar = """(\S)""".r
    acceptMatch("any char", { case Token(singleChar(c), _, _) => c.charAt(0) })
      |< "any char"

  /** A parser of line of a dictionary */
  lazy val dictLine: P[(Char, Tile | (Pos => Cond))] =
    (char <~ optSpaced(minusTk ~ rightChevronTk))
      ~ (tile | singularOp ~ genericMatcher ^^ { case op ~ m => Cond(_, op, m) })
      ^^ { case char ~ tOrM => char -> tOrM }
      |< "dictionary line"

  /** A parser of a shadow command's mode */
  lazy val mode: P[Boolean] =
    (softTk | normalTk)
      ^^ { _.content.toLowerCase == "soft" }
      |< "mode"

}
