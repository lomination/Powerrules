package lomination.powerrules

import scala.util.parsing.input.Position

case class TestPos(l: Int, c: Int) extends Position:
  def line                = l
  def column              = c
  override def toString   = s"$l.$c"
  override def longString = toString
  def lineContents        = ""

object TestPos:
  def apply(l: Int, c: Int): TestPos = new TestPos(l, c)
  def apply(l: Int): TestPos         = new TestPos(l, 0)

object Functions:
  def build[T](tokens: (Position, Position) => T*): Seq[T] =
    tokens.zipWithIndex.map { case f -> i => f.apply(TestPos(i), TestPos(i + 1)) }

object ImplicitConversions:
  given Conversion[Int, TestPos] with
    def apply(l: Int): TestPos = TestPos(l)

  given Conversion[(Int, Int), TestPos] with
    def apply(pos: (Int, Int)): TestPos = TestPos(pos._1, pos._2)
