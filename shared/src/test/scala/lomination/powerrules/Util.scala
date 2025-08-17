package lomination.powerrules

import scala.util.parsing.input.Position
import lomination.powerrules.lexing.tokens.Token

case class TestPos(l: Int) extends Position:
  def line                = l
  def column              = 0
  override def toString   = s"$l"
  override def longString = toString
  def lineContents        = ""

object Functions:
  def build(tokens: (Position, Position) => Token*): Seq[Token] =
    tokens.zipWithIndex.map { case f -> i => f.apply(TestPos(i), TestPos(i + 1))}

  // todo: fix implicit conversion which does not seem to work
  // given Conversion[Int, Position] with
  //   def intToPosition(n: Int): Position = TestPos(n)