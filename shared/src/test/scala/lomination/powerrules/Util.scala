package lomination.powerrules

import lomination.powerrules.lexing.tokens.Token

import scala.util.parsing.input.Position

case class TestPos(l: Int) extends Position:
  def line                = l
  def column              = 0
  override def toString   = s"$l"
  override def longString = toString
  def lineContents        = ""

object Functions:
  def build(tokens: (Position, Position) => Token*): Seq[Token] =
    tokens.zipWithIndex.map { case f -> i => f.apply(TestPos(i), TestPos(i + 1)) }

  given Conversion[Int, TestPos] with
    def apply(n: Int): TestPos = TestPos(n)
