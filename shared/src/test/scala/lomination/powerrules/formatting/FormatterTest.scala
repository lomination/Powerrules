package lomination.powerrules.formatting

import lomination.powerrules.util.*
import lomination.powerrules.lexing.tokens.*
import scala.util.parsing.input.{NoPosition, Position}
import lomination.powerrules.FunSuite
import lomination.powerrules.formatting.Formatter
import lomination.powerrules.config.Config

class FormatterTest extends FunSuite {

  val cfg = Config()

  case class Pos(l: Int) extends Position:
    def line                = l
    def column              = 0
    override def toString   = "<undefined position>"
    override def longString = toString
    def lineContents        = ""

  extension (seq: Seq[StaticTokenFactory[StaticToken]])
    def pos: Seq[StaticToken] =
      seq.zipWithIndex.map { case (tk, i) => tk.apply("", Pos(i), Pos(i + 1)) }

  extension (tk: StaticTokenFactory[StaticToken])
    def pos(l: Int, stop: Int): StaticToken =
      tk.apply("", Pos(l), Pos(stop))

    def pos(l: Int): StaticToken =
      tk.apply("", Pos(l), Pos(l + 1))

    def pos: StaticToken =
      tk.apply("", NoPosition, NoPosition)

  // ---------- Tests ---------- //

  test("FormatterTest - apply method (1)") {
    val test     = Formatter(Seq(Newline, Newline, Newline, Space, Newline, Re).pos)(using cfg)
    val expected = Seq(Newline.pos(4), Re.pos(5))
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("FormatterTest - apply method (2)") {
    val test     = Formatter(Seq(Re, Newline, Space, Space).pos)(using cfg)
    val expected = Seq(Re.pos(0))
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("FormatterTest - apply method (3): indentation error") {
    val test = Formatter(Seq(Re, Newline, Space, Space, Space, With).pos)(using cfg)
    assert(test.isFailure)
  }

  test("FormatterTest - apply method (4): indentation") {
    val test     = Formatter(Seq(Re, Newline, Space, Space, Space, Space, With).pos)(using cfg)
    val expected = Seq(Re.pos(0), Indent.pos(1), With.pos(6), Dedent.pos(7, 7))
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

}
