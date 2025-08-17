package lomination.powerrules.formatting

import lomination.powerrules.util.*
import lomination.powerrules.lexing.tokens.*
import scala.util.parsing.input.{NoPosition, Position}
import lomination.powerrules.FunSuite
import lomination.powerrules.formatting.Formatter
import lomination.powerrules.config.Config
import lomination.powerrules.TestPos
import lomination.powerrules.Functions.build

class FormatterTest extends FunSuite {

  val cfg = Config()

  test("FormatterTest - apply method (1)") {
    val tokens   = build(Newline("\n", _, _), Newline("\n", _, _), Newline("\n", _, _), Space(" ", _, _), Newline("\n", _, _), Literal("re", "re", _, _))
    val test     = Formatter(tokens)(using cfg)
    val expected = Seq(Newline("\n", TestPos(4), TestPos(5)), Literal("re", "re", TestPos(5), TestPos(6)))
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("FormatterTest - apply method (2)") {
    val tokens   = build(Literal("re", "re", _, _), Newline("\n", _, _), Space(" ", _, _), Space(" ", _, _))
    val test     = Formatter(tokens)(using cfg)
    val expected = Seq(Literal("re", "re", TestPos(0), TestPos(1)))
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("FormatterTest - apply method (3): indentation error") {
    val tokens   = build(Literal("re", "re", _, _), Newline("\n", _, _), Space(" ", _, _), Space(" ", _, _), Space(" ", _, _), Literal("with", "with", _, _))
    val test = Formatter(tokens)(using cfg)
    assert(test.isFailure)
  }

  test("FormatterTest - apply method (4): indentation") {
    val tokens   = build(Literal("re", "re", _, _), Newline("\n", _, _), Space(" ", _, _), Space(" ", _, _), Space(" ", _, _), Space(" ", _, _), Literal("with", "with", _, _))
    val test     = Formatter(tokens)(using cfg)
    val expected = Seq(Literal("re", "re", TestPos(0), TestPos(1)), Indent("", TestPos(1), TestPos(2)), Literal("with", "with", TestPos(6), TestPos(7)), Dedent("", TestPos(7), TestPos(7)))
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

}
