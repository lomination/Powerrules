package lomination.powerrules.lexing

import lomination.powerrules.util.style.{ansi0, ansi31, ansi32, ansi33, ansi4}
import lomination.powerrules.lexing.tokens.*
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}
import scala.annotation.targetName

/** This class extends Parsers from scala combinators. It provides a TokenReader, a logger, a type alias P for Parser, a parse function and the |<
  * parser extension which makes logging easier to manage.
  */
class TokenParser extends Parsers {

  override type Elem = Token

  val logger = org.log4s.getLogger

  type P[T] = Parser[T]

  class TokenReader(val tokens: Seq[Token], lastPos: Position) extends Reader[Token] {
    override def first: Token      = tokens.head
    override def atEnd: Boolean    = tokens.isEmpty
    override def pos: Position     = tokens.headOption.map(_.start).getOrElse(lastPos)
    override def rest: TokenReader = new TokenReader(tokens.drop(1), lastPos)
  }

  object TokenReader {
    def apply(tokens: Seq[Token]): TokenReader =
      new TokenReader(tokens, tokens.lastOption.map(_.stop).getOrElse(NoPosition))
  }

  @inline def parse[A](parser: P[A], input: Input): ParseResult[A] = parser(input)

  extension [A](parser: P[A])
    @targetName("logs")
    infix def |<(name: String): P[A] =
      new P[A] {
        def apply(in: Input): ParseResult[A] =
          logger.trace(s"${ansi33}T$ansi0 at $ansi4${in.pos}$ansi0: $name (trying to parse)")
          parser(in) match
            case success @ Success(_, next) =>
              logger.trace(s"${ansi32}S$ansi0 at $ansi4${next.pos}$ansi0: $name (successfully parsed)")
              success
            case failure @ Failure(_, next) =>
              logger.trace(s"${ansi31}F$ansi0 at $ansi4${next.pos}$ansi0: $name (failed to be parsed)")
              failure
            case error @ Error(_, next) =>
              logger.trace(s"${ansi31}E$ansi0 at $ansi4${next.pos}$ansi0: $name (failed to be parsed)")
              error
      } named name

  lazy val defTk: P[Def] =
    acceptMatch("`def` token", { case token: Def => token })
      |< "`def` token"

  lazy val endTk: P[End] =
    acceptMatch("`end` token", { case token: End => token })
      |< "`end` token"

  lazy val replaceTk: P[Replace] =
    acceptMatch("`replace` token", { case token: Replace => token })
      |< "`replace` token"

  lazy val reTk: P[Re] =
    acceptMatch("`re` token", { case token: Re => token })
      |< "`re` token"

  lazy val shadowTk: P[Shadow] =
    acceptMatch("`shadow` token", { case token: Shadow => token })
      |< "`shadow` token"

  lazy val sdTk: P[Sd] =
    acceptMatch("`sd` token", { case token: Sd => token })
      |< "`sd` token"

  lazy val shapeTk: P[Shape] =
    acceptMatch("`shape` token", { case token: Shape => token })
      |< "`shape` token"

  lazy val spTk: P[Sp] =
    acceptMatch("`sp` token", { case token: Sp => token })
      |< "`sp` token"

  lazy val withTk: P[With] =
    acceptMatch("`with` token", { case token: With => token })
      |< "`with` token"

  lazy val withexternalTk: P[Withexternal] =
    acceptMatch("`withexternal` token", { case token: Withexternal => token })
      |< "`withexternal` token"

  lazy val withinternalTk: P[Withinternal] =
    acceptMatch("`withinternal` token", { case token: Withinternal => token })
      |< "`withinternal` token"

  lazy val ifTk: P[If] =
    acceptMatch("`if` token", { case token: If => token })
      |< "`if` token"

  lazy val whenTk: P[When] =
    acceptMatch("`when` token", { case token: When => token })
      |< "`when` token"

  lazy val orTk: P[Or] =
    acceptMatch("`or` token", { case token: Or => token })
      |< "`or` token"

  lazy val randomTk: P[Random] =
    acceptMatch("`random` token", { case token: Random => token })
      |< "`random` token"

  lazy val modeTk: P[Mode] =
    acceptMatch("`mode` token", { case token: Mode => token })
      |< "`mode` token"

  lazy val applyTk: P[Apply] =
    acceptMatch("`apply` token", { case token: Apply => token })
      |< "`apply` token"

  lazy val onTk: P[On] =
    acceptMatch("`on` token", { case token: On => token })
      |< "`on` token"

  lazy val usingTk: P[Using] =
    acceptMatch("`using` token", { case token: Using => token })
      |< "`using` token"

  lazy val thereTk: P[There] =
    acceptMatch("`there` token", { case token: There => token })
      |< "`there` token"

  lazy val isTk: P[Is] =
    acceptMatch("`is` token", { case token: Is => token })
      |< "`is` token"

  lazy val areTk: P[Are] =
    acceptMatch("`are` token", { case token: Are => token })
      |< "`are` token"

  lazy val notTk: P[Not] =
    acceptMatch("`not` token", { case token: Not => token })
      |< "`not` token"

  lazy val fullTk: P[Full] =
    acceptMatch("`full` token", { case token: Full => token })
      |< "`full` token"

  lazy val emptyTk: P[Empty] =
    acceptMatch("`empty` token", { case token: Empty => token })
      |< "`empty` token"

  lazy val edgeTk: P[Edge] =
    acceptMatch("`edge` token", { case token: Edge => token })
      |< "`edge` token"

  lazy val normalTk: P[Normal] =
    acceptMatch("`normal` token", { case token: Normal => token })
      |< "`normal` token"

  lazy val softTk: P[Soft] =
    acceptMatch("`soft` token", { case token: Soft => token })
      |< "`soft` token"

  lazy val outsideTk: P[Outside] =
    acceptMatch("`outside` token", { case token: Outside => token })
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

  lazy val leftAcoladeTk: P[LeftAcolade] =
    acceptMatch("left acolade character `{`", { case token: LeftAcolade => token })
      |< "left acolade character `{`"

  lazy val rightAcoladeTk: P[RightAcolade] =
    acceptMatch("right acolade character `}`", { case token: RightAcolade => token })
      |< "right acolade character `}`"

  lazy val leftChevronTk: P[LeftChevron] =
    acceptMatch("left chevron character `<`", { case token: LeftChevron => token })
      |< "left chevron character `<`"

  lazy val rightChevronTk: P[RightChevron] =
    acceptMatch("right chevron character `>`", { case token: RightChevron => token })
      |< "right chevron character `>`"

  lazy val commaTk: P[Comma] =
    acceptMatch("comma character `,`", { case token: Comma => token })
      |< "comma character `,`"

  lazy val equalTk: P[Equal] =
    acceptMatch("equal sign `=`", { case token: Equal => token })
      |< "equal sign `=`"

  lazy val doulbeQuoteTk: P[DoulbeQuote] =
    acceptMatch("doulbe quote character `\"`", { case token: DoulbeQuote => token })
      |< "doulbe quote character `\"`"

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

  lazy val slashTk: P[Slash] =
    acceptMatch("slash character `/`", { case token: Slash => token })
      |< "slash character `/`"

  lazy val colonTk: P[Colon] =
    acceptMatch("colon character `:`", { case token: Colon => token })
      |< "colon character `:`"

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
