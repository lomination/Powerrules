package lomination.powerrules.lexing

import lomination.powerrules.lexing.tokens._
import lomination.powerrules.util.style.{ansi0, ansi31, ansi32, ansi33, ansi34, ansi4}

import scala.annotation.targetName
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

/** This class extends Parsers from scala combinators. It provides a TokenReader, a logger, a type alias P for Parser, a parse function and the |<
  * parser extension which makes logging easier to manage.
  */
class TokenParser extends Parsers {

  override type Elem = Token

  val logger = org.log4s.getLogger

  type P[T] = Parser[T]

  class TokenReader(tokens: Seq[Token], lastPos: Position) extends Reader[Token] {
    override def first: Token      = tokens.head
    override def atEnd: Boolean    = tokens.isEmpty
    override def pos: Position     = tokens.headOption.map(_.start).getOrElse(lastPos)
    override def rest: TokenReader = new TokenReader(tokens.drop(1), lastPos)
  }

  object TokenReader {
    def apply(tokens: Seq[Token]): TokenReader =
      new TokenReader(tokens, tokens.lastOption.map(_.end).getOrElse(NoPosition))
  }

  @inline
  def parse[A](parser: P[A], input: Input): ParseResult[A] = parser(input)

  extension [A](parser: P[A])
    @targetName("logs")
    infix def |<(name: String): P[A] =
      new P[A] {
        def apply(in: Input): ParseResult[A] =
          logger.trace(s"${ansi34}T$ansi0 at $ansi4${in.pos}$ansi0: $name (trying to parse)")
          parser(in) match
            case success @ Success(_, next) =>
              logger.trace(s"${ansi32}S$ansi0 at $ansi4${next.pos}$ansi0: $name (successfully parsed)")
              success
            case failure @ Failure(_, next) =>
              logger.trace(s"${ansi33}F$ansi0 at $ansi4${next.pos}$ansi0: $name (failed to be parsed)")
              failure
            case error @ Error(_, next) =>
              logger.trace(s"${ansi31}E$ansi0 at $ansi4${next.pos}$ansi0: $name (failed to be parsed)")
              error
      } named name

  lazy val defTk: P[Literal] =
    acceptMatch("`def` token", { case token @ Literal("def", _, _, _) => token })
      |< "`def` token"

  lazy val endTk: P[Literal] =
    acceptMatch("`end` token", { case token @ Literal("end", _, _, _) => token })
      |< "`end` token"

  lazy val replaceTk: P[Literal] =
    acceptMatch("`replace` token", { case token @ Literal("replace", _, _, _) => token })
      |< "`replace` token"

  lazy val reTk: P[Literal] =
    acceptMatch("`re` token", { case token @ Literal("re", _, _, _) => token })
      |< "`re` token"

  lazy val shadowTk: P[Literal] =
    acceptMatch("`shadow` token", { case token @ Literal("shadow", _, _, _) => token })
      |< "`shadow` token"

  lazy val sdTk: P[Literal] =
    acceptMatch("`sd` token", { case token @ Literal("sd", _, _, _) => token })
      |< "`sd` token"

  lazy val shapeTk: P[Literal] =
    acceptMatch("`shape` token", { case token @ Literal("shape", _, _, _) => token })
      |< "`shape` token"

  lazy val spTk: P[Literal] =
    acceptMatch("`sp` token", { case token @ Literal("sp", _, _, _) => token })
      |< "`sp` token"

  lazy val withTk: P[Literal] =
    acceptMatch("`with` token", { case token @ Literal("with", _, _, _) => token })
      |< "`with` token"

  lazy val withexternalTk: P[Literal] =
    acceptMatch("`withexternal` token", { case token @ Literal("withexternal", _, _, _) => token })
      |< "`withexternal` token"

  lazy val withinternalTk: P[Literal] =
    acceptMatch("`withinternal` token", { case token @ Literal("withinternal", _, _, _) => token })
      |< "`withinternal` token"

  lazy val ifTk: P[Literal] =
    acceptMatch("`if` token", { case token @ Literal("if", _, _, _) => token })
      |< "`if` token"

  lazy val whenTk: P[Literal] =
    acceptMatch("`when` token", { case token @ Literal("when", _, _, _) => token })
      |< "`when` token"

  lazy val orTk: P[Literal] =
    acceptMatch("`or` token", { case token @ Literal("or", _, _, _) => token })
      |< "`or` token"

  lazy val randomTk: P[Literal] =
    acceptMatch("`random` token", { case token @ Literal("random", _, _, _) => token })
      |< "`random` token"

  lazy val modeTk: P[Literal] =
    acceptMatch("`mode` token", { case token @ Literal("mode", _, _, _) => token })
      |< "`mode` token"

  lazy val applyTk: P[Literal] =
    acceptMatch("`apply` token", { case token @ Literal("apply", _, _, _) => token })
      |< "`apply` token"

  lazy val onTk: P[Literal] =
    acceptMatch("`on` token", { case token @ Literal("on", _, _, _) => token })
      |< "`on` token"

  lazy val usingTk: P[Literal] =
    acceptMatch("`using` token", { case token @ Literal("using", _, _, _) => token })
      |< "`using` token"

  lazy val thereTk: P[Literal] =
    acceptMatch("`there` token", { case token @ Literal("there", _, _, _) => token })
      |< "`there` token"

  lazy val isTk: P[Literal] =
    acceptMatch("`is` token", { case token @ Literal("is", _, _, _) => token })
      |< "`is` token"

  lazy val areTk: P[Literal] =
    acceptMatch("`are` token", { case token @ Literal("are", _, _, _) => token })
      |< "`are` token"

  lazy val notTk: P[Literal] =
    acceptMatch("`not` token", { case token @ Literal("not", _, _, _) => token })
      |< "`not` token"

  lazy val fullTk: P[Literal] =
    acceptMatch("`full` token", { case token @ Literal("full", _, _, _) => token })
      |< "`full` token"

  lazy val emptyTk: P[Literal] =
    acceptMatch("`empty` token", { case token @ Literal("empty", _, _, _) => token })
      |< "`empty` token"

  lazy val edgeTk: P[Literal] =
    acceptMatch("`edge` token", { case token @ Literal("edge", _, _, _) => token })
      |< "`edge` token"

  lazy val normalTk: P[Literal] =
    acceptMatch("`normal` token", { case token @ Literal("normal", _, _, _) => token })
      |< "`normal` token"

  lazy val softTk: P[Literal] =
    acceptMatch("`soft` token", { case token @ Literal("soft", _, _, _) => token })
      |< "`soft` token"

  lazy val outsideTk: P[Literal] =
    acceptMatch("`outside` token", { case token @ Literal("outside", _, _, _) => token })
      |< "`outside` token"

  lazy val decimalNumberTk: P[DecimalNumber] =
    acceptMatch("decimal number", { case token: DecimalNumber => token })
      |< "decimal number"

  lazy val hexaNumberTk: P[HexaNumber] =
    acceptMatch("hexadecimal number", { case token: HexaNumber => token })
      |< "hexadecimal number"

  lazy val literalTk: P[Literal] =
    acceptMatch("literal token", { case token: Literal => token })
      |< "literal token"

  lazy val plusTk: P[Plus] =
    acceptMatch("plus sign `+`", { case token: Plus => token })
      |< "plus sign `+`"

  lazy val minusTk: P[Minus] =
    acceptMatch("minus sign `-`", { case token: Minus => token })
      |< "minus sign `-`"

  lazy val pipeTk: P[Pipe] =
    acceptMatch("pipe character `|`", { case token: Pipe => token })
      |< "pipe character `|`"

  lazy val starTk: P[Star] =
    acceptMatch("star character `*`", { case token: Star => token })
      |< "star character `*`"

  lazy val percentTk: P[Percent] =
    acceptMatch("percent character `%`", { case token: Percent => token })
      |< "percent character `%`"

  lazy val leftParentheseTk: P[LeftParenthese] =
    acceptMatch("left parenthese character `(`", { case token: LeftParenthese => token })
      |< "left parenthese character `(`"

  lazy val rightParentheseTk: P[RightParenthese] =
    acceptMatch("right parenthese character `)`", { case token: RightParenthese => token })
      |< "right parenthese character `)`"

  lazy val leftBracketTk: P[LeftBracket] =
    acceptMatch("left bracket character `[`", { case token: LeftBracket => token })
      |< "left bracket character `[`"

  lazy val rightBracketTk: P[RightBracket] =
    acceptMatch("right bracket character `]`", { case token: RightBracket => token })
      |< "right bracket character `]`"

  lazy val leftBraceTk: P[LeftBrace] =
    acceptMatch("left brace character `{`", { case token: LeftBrace => token })
      |< "left brace character `{`"

  lazy val rightBraceTk: P[RightBrace] =
    acceptMatch("right brace character `}`", { case token: RightBrace => token })
      |< "right brace character `}`"

  lazy val leftChevronTk: P[LeftChevron] =
    acceptMatch("left chevron character `<`", { case token: LeftChevron => token })
      |< "left chevron character `<`"

  lazy val rightChevronTk: P[RightChevron] =
    acceptMatch("right chevron character `>`", { case token: RightChevron => token })
      |< "right chevron character `>`"

  lazy val commaTk: P[Comma] =
    acceptMatch("comma character `,`", { case token: Comma => token })
      |< "comma character `,`"

  lazy val dollarTk: P[Dollar] =
    acceptMatch("dollar sign `$`", { case token: Dollar => token })
      |< "dollar sign `$`"

  lazy val ampersandTk: P[Ampersand] =
    acceptMatch("ampersand character `&`", { case token: Ampersand => token })
      |< "ampersand character `&`"

  lazy val dotTk: P[Dot] =
    acceptMatch("dot character `.`", { case token: Dot => token })
      |< "dot character `.`"

  lazy val hashtagTk: P[Hashtag] =
    acceptMatch("hashtag character `#`", { case token: Hashtag => token })
      |< "hashtag character `#`"

  lazy val spaceTk: P[Space] =
    acceptMatch("space character ` `", { case token: Space => token })
      |< "space character ` `"

  lazy val newlineTk: P[Newline] =
    acceptMatch("newline character `\\n`", { case token: Newline => token })
      |< "newline character `\\n`"

  lazy val tabTk: P[Tab] =
    acceptMatch("tab character `\\t`", { case token: Tab => token })
      |< "tab character `\\t`"

  lazy val unknownTk: P[Unknown] =
    acceptMatch("unknown token", { case token: Unknown => token })
      |< "unknown token"

  lazy val indentTk: P[Indent] =
    acceptMatch("indent token", { case token: Indent => token })
      |< "indent token"

  lazy val dedentTk: P[Dedent] =
    acceptMatch("dedent token", { case token: Dedent => token })
      |< "dedent token"

}
