package lomination.powerrules.lexing

import lomination.powerrules.config.Config
import lomination.powerrules.lexing.tokens._
import lomination.powerrules.util.style.{ansi0, ansi31, ansi32}
import lomination.powerrules.util.tryMap

import scala.util.Try
import scala.util.parsing.combinator._
import scala.util.parsing.input.Position

object Lexer extends RegexParsers {

  override def skipWhitespace = false

  override val whiteSpace = "".r

  val logger = org.log4s.getLogger

  type P[A] = Parser[A]

  type Unpositioned[A] = (Position, Position) => A

  case class Segment(raw: String, start: Position, end: Position)

  def apply(code: String)(using Config): Try[Seq[Token]] =
    if (code.isEmpty)
      logger trace "Empty source given"
      scala.util.Success(Seq())
    else
      for {
        segments  <- segment(code)
        tokens    <- tokenize(segments)
        evaluated <- CommentHandler(tokens)
      } yield evaluated

  def segment(code: String): Try[Seq[Segment]] =
    parse(segmentsParser, code) match
      case Success(result, next) =>
        scala.util.Success(result)
      case NoSuccess.I(msg, next) =>
        val exception = SegmentationError(s"Semgentation failed at ${next.pos}:\n$msg")
        scala.util.Failure(exception)

  def tokenize(segments: Seq[Segment]): Try[Seq[Token]] =
    segments tryMap { case Segment(raw, start, end) =>
      parse(tokenParser, raw) match
        case Success(result, _) =>
          val token = result.apply(start, end)
          logger.trace(s"Token $ansi32${token.getName}$ansi0 successfully parsed from `${if raw == "\n" then "\\n" else raw}` from $start to $end")
          scala.util.Success(token)
        case NoSuccess.I(msg, _) =>
          logger.error(s"${ansi31}Failed to parse token from `${if raw == "\n" then "\\n" else raw}` from $start to $end ($msg)$ansi0")
          scala.util.Failure(TokenizationError(msg))
    }

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

  // ---------- Parsers ---------- //

  lazy val segmentsParser: P[List[Segment]] =
    phrase(rep(singleSegmentParser))

  lazy val singleSegmentParser: P[Segment] =
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

  // @formatter:off
  
  lazy val tokenParser: P[Unpositioned[Token]] =
    Seq(literalTk, decimalNumberTk, hexaNumberTk, plusTk, minusTk, pipeTk, starTk, percentTk, leftParentheseTk, rightParentheseTk, leftBracketTk, rightBracketTk, leftBraceTk, rightBraceTk, leftChevronTk, rightChevronTk, commaTk, dollarTk, ampersandTk, dotTk, hashtagTk, slashTk, spaceTk, newlineTk, tabTk, unknownTk)
      .map(phrase)
      .reduce((p1, p2) => p1 | p2)
      .named("token")
  
  // @formatter:on

  lazy val literalTk = "[a-zA-Z_][a-zA-Z0-9_]*".r |>> Literal

  lazy val decimalNumberTk = "0|[1-9][0-9]*".r ^^ { Integer.parseInt(_) } |> DecimalNumber

  lazy val hexaNumberTk = "0x[0-9a-fA-F]+".r ^^ { num => Integer.parseInt(num.drop(2), 16) } |> HexaNumber

  lazy val plusTk            = "+" |>> Plus
  lazy val minusTk           = "-" |>> Minus
  lazy val pipeTk            = "|" |>> Pipe
  lazy val starTk            = "*" |>> Star
  lazy val percentTk         = "%" |>> Percent
  lazy val leftParentheseTk  = "(" |>> LeftParenthese
  lazy val rightParentheseTk = ")" |>> RightParenthese
  lazy val leftBracketTk     = "[" |>> LeftBracket
  lazy val rightBracketTk    = "]" |>> RightBracket
  lazy val leftBraceTk       = "{" |>> LeftBrace
  lazy val rightBraceTk      = "}" |>> RightBrace
  lazy val leftChevronTk     = "<" |>> LeftChevron
  lazy val rightChevronTk    = ">" |>> RightChevron
  lazy val commaTk           = "," |>> Comma
  lazy val dollarTk          = "$" |>> Dollar
  lazy val ampersandTk       = "&" |>> Ampersand
  lazy val dotTk             = "." |>> Dot
  lazy val hashtagTk         = "#" |>> Hashtag
  lazy val slashTk           = "/" |>> Slash

  lazy val spaceTk   = " "  |>> Space
  lazy val newlineTk = "\n" |>> Newline
  lazy val tabTk     = "\t" |>> Tab

  lazy val unknownTk = """[\S\s]*""".r |>> Unknown

}
