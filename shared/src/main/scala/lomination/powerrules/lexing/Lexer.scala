package lomination.powerrules.lexing

import scala.util.Try
import scala.util.parsing.combinator.*
import lomination.powerrules.lexing.tokens.*
import lomination.powerrules.config.Config
import lomination.powerrules.util.{dropOnce, i}
import lomination.powerrules.util.style.{ansi0, ansi31, ansi32}
import scala.util.matching.Regex
import scala.annotation.tailrec
import scala.util.parsing.input.{NoPosition, Position}

object Lexer extends RegexParsers {

  override def skipWhitespace = false

  override val whiteSpace = "".r

  val logger = org.log4s.getLogger

  type P[A] = Parser[A]

  type Unpositioned[A] = (Position, Position) => A

  case class Segment(raw: String, start: Position, stop: Position)

  def apply(code: String)(using Config): Try[Seq[Token]] =
    if (code.isEmpty)
      logger trace "Empty source given"
      scala.util.Success(Seq())
    else
      for {
        rawTokens <- scan(code)
        tokens    <- evaluate(rawTokens)
      } yield tokens

  /** Returns raw tokens. Use segmentation in segment of characters of the same "type", i. e. alphanumeric, symbolic, whitespace or unknown. */
  def scan(code: String): Try[Seq[Token]] =
    for {
      segments <- segment(code)
      tokens   <- tokenize(segments)
    } yield tokens

  def segment(code: String): Try[Seq[Segment]] =
    parse(segmentsParser, code) match
      case Success(result, next) =>
        scala.util.Success(result)
      case NoSuccess.I(msg, next) =>
        val exception = SegmentationError(s"Semgentation failed at ${next.pos}:\n$msg")
        scala.util.Failure(exception)

  def tokenize(segments: Seq[Segment]): Try[Seq[Token]] =
    def process(tokens: Seq[Token], segments: Seq[Segment]): Try[Seq[Token]] =
      if (segments.isEmpty) scala.util.Success(tokens)
      else
        val Segment(raw, start, stop) = segments.head
        parse(tokenParser, raw) match
          case Success(result, _) =>
            val token = result.apply(start, stop)
            logger.trace(s"Token $ansi32${token.getName}$ansi0 successfully parsed from `${if raw == "\n" then "\\n" else raw}` from $start to $stop")
            process(tokens :+ token, segments.dropOnce)
          case NoSuccess.I(msg, _) =>
            logger.error(s"${ansi31}Failed to parse token from `${if raw == "\n" then "\\n" else raw}` from $start to $stop ($msg)$ansi0")
            scala.util.Failure(TokenizationError(msg))
    process(Seq(), segments)

  /** Converts raw tokens into tokens with computed indentation and removed comments */
  def evaluate(tokens: Seq[Token]): Try[Seq[Token]] =

    /** Could throw an error when applied on empty seq */
    @tailrec
    def process(cookedOnes: Seq[Token], rawOnes: Seq[Token]): Try[Seq[Token]] =
      rawOnes match
        case Slash(_, _, _) :: Slash(_, _, _) :: next =>
          process(cookedOnes, next.dropWhile(!_.isInstanceOf[Newline]))
        case Slash(_, start, _) :: Star(_, _, _) :: next =>
          dropUnilClosed(next, start) match
            case scala.util.Success(tokens) => process(cookedOnes, tokens)
            case failure                    => failure
        case token :: next =>
          process(cookedOnes :+ token, next)
        case Nil =>
          scala.util.Success(cookedOnes)

    /** Drops until the comment opened at pos is closed */
    @tailrec
    def dropUnilClosed(tokens: Seq[Token], startPos: Position): Try[Seq[Token]] =
      tokens match
        case Star(_, _, _) :: Slash(_, _, _) :: next =>
          scala.util.Success(next)
        case token :: next =>
          dropUnilClosed(next, startPos)
        case Nil =>
          scala.util.Failure(CommentError(s"Comment opened at $startPos not closed"))

    process(Seq(), tokens)

  // ---------- Extensions ---------- //

  extension [A](parser: P[A])

    def |>[B <: NonStaticToken[A]](factory: NonStaticTokenFactory[A, B]): P[Unpositioned[B]] =
      new P[Unpositioned[B]] {
        def apply(in: Input): ParseResult[Unpositioned[B]] =
          parser(in) match
            case Success(result, next) =>
              val raw   = in.source.subSequence(in.offset, next.offset).toString
              val token = factory(result, raw, _, _)
              Success(token, next)
            case NoSuccess.I(msg, next) =>
              Failure(msg, next)
      }

    def |>>[B <: StaticToken](factory: StaticTokenFactory[B]): P[Unpositioned[B]] =
      new P[Unpositioned[B]] {
        def apply(in: Input): ParseResult[Unpositioned[B]] =
          parser(in) match
            case Success(result, next) =>
              val raw   = in.source.subSequence(in.offset, next.offset).toString
              val token = factory(raw, _, _)
              Success(token, next)
            case NoSuccess.I(msg, next) =>
              Failure(msg, next)
      }

    def ¦[B](other: P[B]): P[A | B] =
      parser | phrase(other)

  // ---------- Parsers ---------- //

  lazy val segmentsParser: P[List[Segment]] =
    phrase(rep(segmentParser))

  lazy val segmentParser: P[Segment] =
    new P[Segment] {
      val regex = """[a-zA-Z_][a-zA-Z0-9_]*|[0-9][a-zA-Z0-9]*|[\S\s]""".r
      def apply(in: Input): ParseResult[Segment] =
        parse(regex, in) match
          case Success(result, next) =>
            val raw = in.source.subSequence(in.offset, next.offset).toString
            Success(Segment(raw, in.pos, next.pos), next)
          case noSuccess: NoSuccess =>
            noSuccess
    }

  lazy val tokenParser: P[Unpositioned[Token]] =
    // @formatter:off
    phrase(defTk) ¦ endTk ¦ replaceTk ¦ reTk ¦ shadowTk ¦ sdTk ¦ shapeTk ¦ spTk ¦ withTk ¦ withexternalTk ¦ withinternalTk ¦ ifTk ¦ whenTk ¦ orTk ¦ randomTk ¦ modeTk ¦ applyTk ¦ onTk ¦ usingTk ¦ thereTk ¦ isTk ¦ areTk ¦ notTk ¦ fullTk ¦ emptyTk ¦ edgeTk ¦ normalTk ¦ softTk ¦ outsideTk ¦ literalTk ¦ decimalNumberTk ¦ hexaNumberTk ¦ plusTk ¦ minusTk ¦ pipeTk ¦ starTk ¦ percentTk ¦ leftParentheseTk ¦ rightParentheseTk ¦ leftBracketTk ¦ rightBracketTk ¦ leftAcoladeTk ¦ rightAcoladeTk ¦ leftChevronTk ¦ rightChevronTk ¦ commaTk ¦ doubleQuoteTk ¦ dollarTk ¦ ampersandTk ¦ dotTk ¦ hashtagTk ¦ slashTk ¦ spaceTk ¦ newlineTk ¦ tabTk ¦ unknownTk named "token"
    // @formatter:on

  lazy val defTk          = "def".i          |>> Def          named "keyword `def`"
  lazy val endTk          = "end".i          |>> End          named "keyword `end`"
  lazy val replaceTk      = "replace".i      |>> Replace      named "keyword `replace`"
  lazy val reTk           = "re".i           |>> Re           named "keyword `re`"
  lazy val shadowTk       = "shadow".i       |>> Shadow       named "keyword `shadow`"
  lazy val sdTk           = "sd".i           |>> Sd           named "keyword `sd`"
  lazy val shapeTk        = "shape".i        |>> Shape        named "keyword `shape`"
  lazy val spTk           = "sp".i           |>> Sp           named "keyword `sp`"
  lazy val withTk         = "with".i         |>> With         named "keyword `with`"
  lazy val withexternalTk = "withexternal".i |>> Withexternal named "keyword `withexternal`"
  lazy val withinternalTk = "withinternal".i |>> Withinternal named "keyword `withinternal`"
  lazy val ifTk           = "if".i           |>> If           named "keyword `if`"
  lazy val whenTk         = "when".i         |>> When         named "keyword `when`"
  lazy val orTk           = "or".i           |>> Or           named "keyword `or`"
  lazy val randomTk       = "random".i       |>> Random       named "keyword `random`"
  lazy val modeTk         = "mode".i         |>> Mode         named "keyword `mode`"
  lazy val applyTk        = "apply".i        |>> Apply        named "keyword `apply`"
  lazy val onTk           = "on".i           |>> On           named "keyword `on`"
  lazy val usingTk        = "using".i        |>> Using        named "keyword `using`"
  lazy val thereTk        = "there".i        |>> There        named "keyword `there`"
  lazy val isTk           = "is".i           |>> Is           named "keyword `is`"
  lazy val areTk          = "are".i          |>> Are          named "keyword `are`"
  lazy val notTk          = "not".i          |>> Not          named "keyword `not`"
  lazy val fullTk         = "full".i         |>> Full         named "keyword `full`"
  lazy val emptyTk        = "empty".i        |>> Empty        named "keyword `empty`"
  lazy val edgeTk         = "edge".i         |>> Edge         named "keyword `edge`"
  lazy val normalTk       = "normal".i       |>> Normal       named "keyword `normal`"
  lazy val softTk         = "soft".i         |>> Soft         named "keyword `soft`"
  lazy val outsideTk      = "outside".i      |>> Outside      named "keyword `outside`"

  lazy val literalTk = "[a-zA-Z_][a-zA-Z0-9_]*".r |> Literal

  lazy val decimalNumberTk = "0|[1-9][0-9]*".r ^^ { Integer.parseInt(_) } |> DecimalNumber

  lazy val hexaNumberTk = "0x[0-9a-fA-F]+".r ^^ { num => Integer.parseInt(num.drop(2), 16) } |> HexaNumber

  lazy val plusTk            = "+"  |>> Plus
  lazy val minusTk           = "-"  |>> Minus
  lazy val pipeTk            = "|"  |>> Pipe
  lazy val starTk            = "*"  |>> Star
  lazy val percentTk         = "%"  |>> Percent
  lazy val leftParentheseTk  = "("  |>> LeftParenthese
  lazy val rightParentheseTk = ")"  |>> RightParenthese
  lazy val leftBracketTk     = "["  |>> LeftBracket
  lazy val rightBracketTk    = "]"  |>> RightBracket
  lazy val leftAcoladeTk     = "{"  |>> LeftAcolade
  lazy val rightAcoladeTk    = "}"  |>> RightAcolade
  lazy val leftChevronTk     = "<"  |>> LeftChevron
  lazy val rightChevronTk    = ">"  |>> RightChevron
  lazy val commaTk           = ","  |>> Comma
  lazy val doubleQuoteTk     = "\"" |>> DoulbeQuote
  lazy val dollarTk          = "$"  |>> Dollar
  lazy val ampersandTk       = "&"  |>> Ampersand
  lazy val dotTk             = "."  |>> Dot
  lazy val hashtagTk         = "#"  |>> Hashtag
  lazy val slashTk           = "/"  |>> Slash
  lazy val colonTk           = ":"  |>> Colon

  lazy val spaceTk   = " "  |>> Space
  lazy val newlineTk = "\n" |>> Newline
  lazy val tabTk     = "\t" |>> Tab

  lazy val unknownTk = """[\S\s]*""".r |>> Unknown

}
