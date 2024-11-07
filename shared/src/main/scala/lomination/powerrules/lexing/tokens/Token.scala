package lomination.powerrules.lexing.tokens

import scala.util.parsing.input.Position
import lomination.powerrules.lexing.tokens.Literal

sealed trait Token:
  val raw: String
  val start: Position
  val stop: Position
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

case class Def(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Def extends StaticTokenFactory[Def] {
  def apply(raw: String, start: Position, stop: Position) = new Def(raw, start, stop)
}

case class End(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object End extends StaticTokenFactory[End] {
  def apply(raw: String, start: Position, stop: Position) = new End(raw, start, stop)
}

case class Replace(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Replace extends StaticTokenFactory[Replace] {
  def apply(raw: String, start: Position, stop: Position) = new Replace(raw, start, stop)
}

case class Re(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Re extends StaticTokenFactory[Re] {
  def apply(raw: String, start: Position, stop: Position) = new Re(raw, start, stop)
}

case class Shadow(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Shadow extends StaticTokenFactory[Shadow] {
  def apply(raw: String, start: Position, stop: Position) = new Shadow(raw, start, stop)
}

case class Sd(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Sd extends StaticTokenFactory[Sd] {
  def apply(raw: String, start: Position, stop: Position) = new Sd(raw, start, stop)
}

case class Shape(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Shape extends StaticTokenFactory[Shape] {
  def apply(raw: String, start: Position, stop: Position) = new Shape(raw, start, stop)
}

case class Sp(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Sp extends StaticTokenFactory[Sp] {
  def apply(raw: String, start: Position, stop: Position) = new Sp(raw, start, stop)
}

case class With(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object With extends StaticTokenFactory[With] {
  def apply(raw: String, start: Position, stop: Position) = new With(raw, start, stop)
}

case class Withexternal(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Withexternal extends StaticTokenFactory[Withexternal] {
  def apply(raw: String, start: Position, stop: Position) = new Withexternal(raw, start, stop)
}

case class Withinternal(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Withinternal extends StaticTokenFactory[Withinternal] {
  def apply(raw: String, start: Position, stop: Position) = new Withinternal(raw, start, stop)
}

case class If(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object If extends StaticTokenFactory[If] {
  def apply(raw: String, start: Position, stop: Position) = new If(raw, start, stop)
}

case class When(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object When extends StaticTokenFactory[When] {
  def apply(raw: String, start: Position, stop: Position) = new When(raw, start, stop)
}

case class Or(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Or extends StaticTokenFactory[Or] {
  def apply(raw: String, start: Position, stop: Position) = new Or(raw, start, stop)
}

case class Random(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Random extends StaticTokenFactory[Random] {
  def apply(raw: String, start: Position, stop: Position) = new Random(raw, start, stop)
}

case class Mode(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Mode extends StaticTokenFactory[Mode] {
  def apply(raw: String, start: Position, stop: Position) = new Mode(raw, start, stop)
}

case class Apply(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Apply extends StaticTokenFactory[Apply] {
  def apply(raw: String, start: Position, stop: Position) = new Apply(raw, start, stop)
}

case class On(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object On extends StaticTokenFactory[On] {
  def apply(raw: String, start: Position, stop: Position) = new On(raw, start, stop)
}

case class Using(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Using extends StaticTokenFactory[Using] {
  def apply(raw: String, start: Position, stop: Position) = new Using(raw, start, stop)
}

case class There(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object There extends StaticTokenFactory[There] {
  def apply(raw: String, start: Position, stop: Position) = new There(raw, start, stop)
}

case class Is(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Is extends StaticTokenFactory[Is] {
  def apply(raw: String, start: Position, stop: Position) = new Is(raw, start, stop)
}

case class Are(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Are extends StaticTokenFactory[Are] {
  def apply(raw: String, start: Position, stop: Position) = new Are(raw, start, stop)
}

case class Not(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Not extends StaticTokenFactory[Not] {
  def apply(raw: String, start: Position, stop: Position) = new Not(raw, start, stop)
}

case class Full(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Full extends StaticTokenFactory[Full] {
  def apply(raw: String, start: Position, stop: Position) = new Full(raw, start, stop)
}

case class Empty(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Empty extends StaticTokenFactory[Empty] {
  def apply(raw: String, start: Position, stop: Position) = new Empty(raw, start, stop)
}

case class Edge(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Edge extends StaticTokenFactory[Edge] {
  def apply(raw: String, start: Position, stop: Position) = new Edge(raw, start, stop)
}

case class Normal(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Normal extends StaticTokenFactory[Normal] {
  def apply(raw: String, start: Position, stop: Position) = new Normal(raw, start, stop)
}

case class Soft(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Soft extends StaticTokenFactory[Soft] {
  def apply(raw: String, start: Position, stop: Position) = new Soft(raw, start, stop)
}

case class Outside(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Outside extends StaticTokenFactory[Outside] {
  def apply(raw: String, start: Position, stop: Position) = new Outside(raw, start, stop)
}

case class Literal(content: String, raw: String, start: Position, stop: Position) extends NonStaticToken[String](content, raw, start, stop)
object Literal extends NonStaticTokenFactory[String, Literal] {
  def apply(content: String, raw: String, start: Position, stop: Position) = new Literal(content, raw, start, stop)
}

case class DecimalNumber(content: Int, raw: String, start: Position, stop: Position) extends NonStaticToken[Int](content, raw, start, stop)
object DecimalNumber extends NonStaticTokenFactory[Int, DecimalNumber] {
  def apply(content: Int, raw: String, start: Position, stop: Position) = new DecimalNumber(content, raw, start, stop)
}

case class HexaNumber(content: Int, raw: String, start: Position, stop: Position) extends NonStaticToken[Int](content, raw, start, stop)
object HexaNumber extends NonStaticTokenFactory[Int, HexaNumber] {
  def apply(content: Int, raw: String, start: Position, stop: Position) = new HexaNumber(content, raw, start, stop)
}

// ---------- Special characters token parsers ---------- //

case class Plus(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Plus extends StaticTokenFactory[Plus] {
  def apply(raw: String, start: Position, stop: Position) = new Plus(raw, start, stop)
}

case class Minus(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Minus extends StaticTokenFactory[Minus] {
  def apply(raw: String, start: Position, stop: Position) = new Minus(raw, start, stop)
}

case class Pipe(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Pipe extends StaticTokenFactory[Pipe] {
  def apply(raw: String, start: Position, stop: Position) = new Pipe(raw, start, stop)
}

case class Star(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Star extends StaticTokenFactory[Star] {
  def apply(raw: String, start: Position, stop: Position) = new Star(raw, start, stop)
}

case class Percent(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Percent extends StaticTokenFactory[Percent] {
  def apply(raw: String, start: Position, stop: Position) = new Percent(raw, start, stop)
}

case class LeftParenthese(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object LeftParenthese extends StaticTokenFactory[LeftParenthese] {
  def apply(raw: String, start: Position, stop: Position) = new LeftParenthese(raw, start, stop)
}

case class RightParenthese(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object RightParenthese extends StaticTokenFactory[RightParenthese] {
  def apply(raw: String, start: Position, stop: Position) = new RightParenthese(raw, start, stop)
}

case class LeftBracket(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object LeftBracket extends StaticTokenFactory[LeftBracket] {
  def apply(raw: String, start: Position, stop: Position) = new LeftBracket(raw, start, stop)
}

case class RightBracket(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object RightBracket extends StaticTokenFactory[RightBracket] {
  def apply(raw: String, start: Position, stop: Position) = new RightBracket(raw, start, stop)
}

case class LeftAcolade(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object LeftAcolade extends StaticTokenFactory[LeftAcolade] {
  def apply(raw: String, start: Position, stop: Position) = new LeftAcolade(raw, start, stop)
}

case class RightAcolade(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object RightAcolade extends StaticTokenFactory[RightAcolade] {
  def apply(raw: String, start: Position, stop: Position) = new RightAcolade(raw, start, stop)
}

case class LeftChevron(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object LeftChevron extends StaticTokenFactory[LeftChevron] {
  def apply(raw: String, start: Position, stop: Position) = new LeftChevron(raw, start, stop)
}

case class RightChevron(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object RightChevron extends StaticTokenFactory[RightChevron] {
  def apply(raw: String, start: Position, stop: Position) = new RightChevron(raw, start, stop)
}

case class Comma(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Comma extends StaticTokenFactory[Comma] {
  def apply(raw: String, start: Position, stop: Position) = new Comma(raw, start, stop)
}

case class DoulbeQuote(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object DoulbeQuote extends StaticTokenFactory[DoulbeQuote] {
  def apply(raw: String, start: Position, stop: Position) = new DoulbeQuote(raw, start, stop)
}

case class Dollar(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Dollar extends StaticTokenFactory[Dollar] {
  def apply(raw: String, start: Position, stop: Position) = new Dollar(raw, start, stop)
}

case class Ampersand(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Ampersand extends StaticTokenFactory[Ampersand] {
  def apply(raw: String, start: Position, stop: Position) = new Ampersand(raw, start, stop)
}

case class Dot(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Dot extends StaticTokenFactory[Dot] {
  def apply(raw: String, start: Position, stop: Position) = new Dot(raw, start, stop)
}

case class Hashtag(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Hashtag extends StaticTokenFactory[Hashtag] {
  def apply(raw: String, start: Position, stop: Position) = new Hashtag(raw, start, stop)
}

case class Slash(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Slash extends StaticTokenFactory[Slash] {
  def apply(raw: String, start: Position, stop: Position) = new Slash(raw, start, stop)
}

case class Colon(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Colon extends StaticTokenFactory[Colon] {
  def apply(raw: String, start: Position, stop: Position) = new Colon(raw, start, stop)
}

// ---------- Whitespace token parsers ---------- //

case class Space(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Space extends StaticTokenFactory[Space] {
  def apply(raw: String, start: Position, stop: Position) = new Space(raw, start, stop)
}

case class Newline(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Newline extends StaticTokenFactory[Newline] {
  def apply(raw: String, start: Position, stop: Position) = new Newline(raw, start, stop)
}

case class Tab(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Tab extends StaticTokenFactory[Tab] {
  def apply(raw: String, start: Position, stop: Position) = new Tab(raw, start, stop)
}

// ---------- Unknown ---------- //

case class Unknown(raw: String, start: Position, stop: Position) extends StaticToken(raw, start, stop)
object Unknown extends StaticTokenFactory[Unknown] {
  def apply(raw: String, start: Position, stop: Position) = new Unknown(raw, start, stop)
}

// ---------- Non parsable tokens ---------- //

case class Indent(start: Position, stop: Position) extends StaticToken("", start, stop) { val raw = "" }
object Indent extends StaticTokenFactory[Indent] {
  def apply(raw: String, start: Position, stop: Position) = new Indent(start, stop)
}

case class Dedent(start: Position, stop: Position) extends StaticToken("", start, stop) { val raw = "" }
object Dedent extends StaticTokenFactory[Dedent] {
  def apply(raw: String, start: Position, stop: Position) = new Dedent(start, stop)
}
