package lomination.powerrules.lexing.tokens

import scala.util.parsing.input.Position

sealed trait Token:
  val raw: String
  val start: Position
  val stop: Position // todo: "end" could be better than "stop"
  /** Returns a this token with updates start and stop positions.
    *
    * @param start
    *   the new desired value for the start position of this token.
    * @param stop
    *   the new desired value for the start position of this token.
    * @return
    *   a new token with `start` as start position and `stop` as stop position.
    */
  def repos(start: Position, stop: Position): Token
  def getName: String =
    this.getClass.getSimpleName

object Token:
  def unapply(token: Token): Option[(String, Position, Position)] =
    Some(token.raw, token.start, token.stop)

abstract class NonStaticToken[+A](a: A, raw: String, start: Position, stop: Position) extends Token

abstract class NonStaticTokenFactory[-A, +B <: NonStaticToken[A]] extends ((A, String, Position, Position) => B)

abstract class StaticToken(raw: String, start: Position, stop: Position) extends Token

abstract class StaticTokenFactory[+A <: StaticToken] extends ((String, Position, Position) => A)

// ---------- Alphanumeric token parsers ---------- //

case class Literal(content: String, raw: String, start: Position, stop: Position) extends NonStaticToken[String](content, raw, start, stop):
  def repos(start: Position, stop: Position): Token = new Literal(content, raw, start, stop)

object Literal extends NonStaticTokenFactory[String, Literal]:
  def apply(content: String, raw: String, start: Position, stop: Position) = new Literal(content, raw, start, stop)

case class DecimalNumber(content: Int, raw: String, start: Position, stop: Position) extends NonStaticToken[Int](content, raw, start, stop):
  def repos(start: Position, stop: Position): Token = new DecimalNumber(content, raw, start, stop)

object DecimalNumber extends NonStaticTokenFactory[Int, DecimalNumber]:
  def apply(content: Int, raw: String, start: Position, stop: Position) = new DecimalNumber(content, raw, start, stop)

case class HexaNumber(content: Int, raw: String, start: Position, stop: Position) extends NonStaticToken[Int](content, raw, start, stop):
  def repos(start: Position, stop: Position): Token = new HexaNumber(content, raw, start, stop)

object HexaNumber extends NonStaticTokenFactory[Int, HexaNumber]:
  def apply(content: Int, raw: String, start: Position, stop: Position) = new HexaNumber(content, raw, start, stop)

// ---------- Special characters token parsers ---------- //

case class Plus(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new Plus(raw, start, stop)

object Plus extends StaticTokenFactory[Plus]:
  def apply(raw: String, start: Position, stop: Position) = new Plus(raw, start, stop)

case class Minus(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new Minus(raw, start, stop)

object Minus extends StaticTokenFactory[Minus]:
  def apply(raw: String, start: Position, stop: Position) = new Minus(raw, start, stop)

case class Pipe(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new Pipe(raw, start, stop)

object Pipe extends StaticTokenFactory[Pipe]:
  def apply(raw: String, start: Position, stop: Position) = new Pipe(raw, start, stop)

case class Star(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new Star(raw, start, stop)

object Star extends StaticTokenFactory[Star]:
  def apply(raw: String, start: Position, stop: Position) = new Star(raw, start, stop)

case class Percent(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new Percent(raw, start, stop)

object Percent extends StaticTokenFactory[Percent]:
  def apply(raw: String, start: Position, stop: Position) = new Percent(raw, start, stop)

case class LeftParenthese(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new LeftParenthese(raw, start, stop)

object LeftParenthese extends StaticTokenFactory[LeftParenthese]:
  def apply(raw: String, start: Position, stop: Position) = new LeftParenthese(raw, start, stop)

case class RightParenthese(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new RightParenthese(raw, start, stop)

object RightParenthese extends StaticTokenFactory[RightParenthese]:
  def apply(raw: String, start: Position, stop: Position) = new RightParenthese(raw, start, stop)

case class LeftBracket(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new LeftBracket(raw, start, stop)

object LeftBracket extends StaticTokenFactory[LeftBracket]:
  def apply(raw: String, start: Position, stop: Position) = new LeftBracket(raw, start, stop)

case class RightBracket(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new RightBracket(raw, start, stop)

object RightBracket extends StaticTokenFactory[RightBracket]:
  def apply(raw: String, start: Position, stop: Position) = new RightBracket(raw, start, stop)

case class LeftAcolade(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new LeftAcolade(raw, start, stop)

object LeftAcolade extends StaticTokenFactory[LeftAcolade]:
  def apply(raw: String, start: Position, stop: Position) = new LeftAcolade(raw, start, stop)

case class RightAcolade(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new RightAcolade(raw, start, stop)

object RightAcolade extends StaticTokenFactory[RightAcolade]:
  def apply(raw: String, start: Position, stop: Position) = new RightAcolade(raw, start, stop)

case class LeftChevron(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new LeftChevron(raw, start, stop)

object LeftChevron extends StaticTokenFactory[LeftChevron]:
  def apply(raw: String, start: Position, stop: Position) = new LeftChevron(raw, start, stop)

case class RightChevron(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new RightChevron(raw, start, stop)

object RightChevron extends StaticTokenFactory[RightChevron]:
  def apply(raw: String, start: Position, stop: Position) = new RightChevron(raw, start, stop)

case class Comma(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new Comma(raw, start, stop)

object Comma extends StaticTokenFactory[Comma]:
  def apply(raw: String, start: Position, stop: Position) = new Comma(raw, start, stop)

case class Dollar(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new Dollar(raw, start, stop)

object Dollar extends StaticTokenFactory[Dollar]:
  def apply(raw: String, start: Position, stop: Position) = new Dollar(raw, start, stop)

case class Ampersand(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new Ampersand(raw, start, stop)

object Ampersand extends StaticTokenFactory[Ampersand]:
  def apply(raw: String, start: Position, stop: Position) = new Ampersand(raw, start, stop)

case class Dot(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new Dot(raw, start, stop)

object Dot extends StaticTokenFactory[Dot]:
  def apply(raw: String, start: Position, stop: Position) = new Dot(raw, start, stop)

case class Hashtag(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new Hashtag(raw, start, stop)

object Hashtag extends StaticTokenFactory[Hashtag]:
  def apply(raw: String, start: Position, stop: Position) = new Hashtag(raw, start, stop)

case class Slash(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new Slash(raw, start, stop)

// ---------- Whitespace token parsers ---------- //

case class Space(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new Space(raw, start, stop)

object Space extends StaticTokenFactory[Space]:
  def apply(raw: String, start: Position, stop: Position) = new Space(raw, start, stop)

case class Newline(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new Newline(raw, start, stop)

object Newline extends StaticTokenFactory[Newline]:
  def apply(raw: String, start: Position, stop: Position) = new Newline(raw, start, stop)

case class Tab(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new Tab(raw, start, stop)

object Tab extends StaticTokenFactory[Tab]:
  def apply(raw: String, start: Position, stop: Position) = new Tab(raw, start, stop)

// ---------- Unknown ---------- //

case class Unknown(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop):
  def repos(start: Position, stop: Position): Token = new Unknown(raw, start, stop)

object Unknown extends StaticTokenFactory[Unknown]:
  def apply(raw: String, start: Position, stop: Position) = new Unknown(raw, start, stop)

// ---------- Non parsable tokens ---------- //

case class Indent(start: Position, stop: Position) extends StaticToken("", start, stop):
  val raw                                           = ""
  def repos(start: Position, stop: Position): Token = new Indent(start, stop)

object Indent extends StaticTokenFactory[Indent]:
  def apply(raw: String, start: Position, stop: Position) = new Indent(start, stop)

case class Dedent(start: Position, stop: Position) extends StaticToken("", start, stop):
  val raw                                           = ""
  def repos(start: Position, stop: Position): Token = new Dedent(start, stop)

object Dedent extends StaticTokenFactory[Dedent]:
  def apply(raw: String, start: Position, stop: Position) = new Dedent(start, stop)
