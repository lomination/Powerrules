package lomination.powerrules.lexing.tokens

import scala.util.parsing.input.Position

sealed trait Token:
  val raw: String
  val start: Position
  val end: Position

  /** Returns a this token with updates start and end positions.
    *
    * @param start
    *   the new desired value for the start position of this token.
    * @param end
    *   the new desired value for the start position of this token.
    * @return
    *   a new token with `start` as start position and `end` as end position.
    */
  def repos(start: Position, end: Position): Token
  def getName: String =
    this.getClass.getSimpleName

object Token:
  def unapply(token: Token): Option[(String, Position, Position)] =
    Some(token.raw, token.start, token.end)

abstract class NonStaticToken[+A](a: A, raw: String, start: Position, end: Position) extends Token

abstract class NonStaticTokenFactory[-A, +B <: NonStaticToken[A]] extends ((A, String, Position, Position) => B)

abstract class StaticToken(raw: String, start: Position, end: Position) extends Token

abstract class StaticTokenFactory[+A <: StaticToken] extends ((String, Position, Position) => A)

// ---------- Alphanumeric token parsers ---------- //

case class Literal(content: String, raw: String, start: Position, end: Position) extends NonStaticToken[String](content, raw, start, end):
  def repos(start: Position, end: Position): Token = new Literal(content, raw, start, end)

object Literal extends NonStaticTokenFactory[String, Literal]:
  def apply(content: String, raw: String, start: Position, end: Position) = new Literal(content, raw, start, end)

case class DecimalNumber(content: Int, raw: String, start: Position, end: Position) extends NonStaticToken[Int](content, raw, start, end):
  def repos(start: Position, end: Position): Token = new DecimalNumber(content, raw, start, end)

object DecimalNumber extends NonStaticTokenFactory[Int, DecimalNumber]:
  def apply(content: Int, raw: String, start: Position, end: Position) = new DecimalNumber(content, raw, start, end)

case class HexaNumber(content: Int, raw: String, start: Position, end: Position) extends NonStaticToken[Int](content, raw, start, end):
  def repos(start: Position, end: Position): Token = new HexaNumber(content, raw, start, end)

object HexaNumber extends NonStaticTokenFactory[Int, HexaNumber]:
  def apply(content: Int, raw: String, start: Position, end: Position) = new HexaNumber(content, raw, start, end)

// ---------- Special characters token parsers ---------- //

case class Plus(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new Plus(raw, start, end)

object Plus extends StaticTokenFactory[Plus]:
  def apply(raw: String, start: Position, end: Position) = new Plus(raw, start, end)

case class Minus(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new Minus(raw, start, end)

object Minus extends StaticTokenFactory[Minus]:
  def apply(raw: String, start: Position, end: Position) = new Minus(raw, start, end)

case class Pipe(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new Pipe(raw, start, end)

object Pipe extends StaticTokenFactory[Pipe]:
  def apply(raw: String, start: Position, end: Position) = new Pipe(raw, start, end)

case class Star(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new Star(raw, start, end)

object Star extends StaticTokenFactory[Star]:
  def apply(raw: String, start: Position, end: Position) = new Star(raw, start, end)

case class Percent(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new Percent(raw, start, end)

object Percent extends StaticTokenFactory[Percent]:
  def apply(raw: String, start: Position, end: Position) = new Percent(raw, start, end)

case class LeftParenthese(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new LeftParenthese(raw, start, end)

object LeftParenthese extends StaticTokenFactory[LeftParenthese]:
  def apply(raw: String, start: Position, end: Position) = new LeftParenthese(raw, start, end)

case class RightParenthese(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new RightParenthese(raw, start, end)

object RightParenthese extends StaticTokenFactory[RightParenthese]:
  def apply(raw: String, start: Position, end: Position) = new RightParenthese(raw, start, end)

case class LeftBracket(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new LeftBracket(raw, start, end)

object LeftBracket extends StaticTokenFactory[LeftBracket]:
  def apply(raw: String, start: Position, end: Position) = new LeftBracket(raw, start, end)

case class RightBracket(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new RightBracket(raw, start, end)

object RightBracket extends StaticTokenFactory[RightBracket]:
  def apply(raw: String, start: Position, end: Position) = new RightBracket(raw, start, end)

case class LeftAcolade(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new LeftAcolade(raw, start, end)

object LeftAcolade extends StaticTokenFactory[LeftAcolade]:
  def apply(raw: String, start: Position, end: Position) = new LeftAcolade(raw, start, end)

case class RightAcolade(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new RightAcolade(raw, start, end)

object RightAcolade extends StaticTokenFactory[RightAcolade]:
  def apply(raw: String, start: Position, end: Position) = new RightAcolade(raw, start, end)

case class LeftChevron(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new LeftChevron(raw, start, end)

object LeftChevron extends StaticTokenFactory[LeftChevron]:
  def apply(raw: String, start: Position, end: Position) = new LeftChevron(raw, start, end)

case class RightChevron(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new RightChevron(raw, start, end)

object RightChevron extends StaticTokenFactory[RightChevron]:
  def apply(raw: String, start: Position, end: Position) = new RightChevron(raw, start, end)

case class Comma(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new Comma(raw, start, end)

object Comma extends StaticTokenFactory[Comma]:
  def apply(raw: String, start: Position, end: Position) = new Comma(raw, start, end)

case class Dollar(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new Dollar(raw, start, end)

object Dollar extends StaticTokenFactory[Dollar]:
  def apply(raw: String, start: Position, end: Position) = new Dollar(raw, start, end)

case class Ampersand(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new Ampersand(raw, start, end)

object Ampersand extends StaticTokenFactory[Ampersand]:
  def apply(raw: String, start: Position, end: Position) = new Ampersand(raw, start, end)

case class Dot(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new Dot(raw, start, end)

object Dot extends StaticTokenFactory[Dot]:
  def apply(raw: String, start: Position, end: Position) = new Dot(raw, start, end)

case class Hashtag(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new Hashtag(raw, start, end)

object Hashtag extends StaticTokenFactory[Hashtag]:
  def apply(raw: String, start: Position, end: Position) = new Hashtag(raw, start, end)

case class Slash(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new Slash(raw, start, end)

// ---------- Whitespace token parsers ---------- //

case class Space(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new Space(raw, start, end)

object Space extends StaticTokenFactory[Space]:
  def apply(raw: String, start: Position, end: Position) = new Space(raw, start, end)

case class Newline(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new Newline(raw, start, end)

object Newline extends StaticTokenFactory[Newline]:
  def apply(raw: String, start: Position, end: Position) = new Newline(raw, start, end)

case class Tab(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new Tab(raw, start, end)

object Tab extends StaticTokenFactory[Tab]:
  def apply(raw: String, start: Position, end: Position) = new Tab(raw, start, end)

// ---------- Unknown ---------- //

case class Unknown(raw: String, start: Position, end: Position) extends StaticToken(raw, start, end):
  def repos(start: Position, end: Position): Token = new Unknown(raw, start, end)

object Unknown extends StaticTokenFactory[Unknown]:
  def apply(raw: String, start: Position, end: Position) = new Unknown(raw, start, end)

// ---------- Non parsable tokens ---------- //

case class Indent(start: Position, end: Position) extends StaticToken("", start, end):
  val raw                                          = ""
  def repos(start: Position, end: Position): Token = new Indent(start, end)

object Indent extends StaticTokenFactory[Indent]:
  def apply(raw: String, start: Position, end: Position) = new Indent(start, end)

case class Dedent(start: Position, end: Position) extends StaticToken("", start, end):
  val raw                                          = ""
  def repos(start: Position, end: Position): Token = new Dedent(start, end)

object Dedent extends StaticTokenFactory[Dedent]:
  def apply(raw: String, start: Position, end: Position) = new Dedent(start, end)
