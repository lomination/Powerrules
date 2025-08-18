package lomination.powerrules.formatting

import lomination.powerrules.FunSuite
import lomination.powerrules.Functions.build
import lomination.powerrules.Functions.given
import lomination.powerrules.config.Config
import lomination.powerrules.lexing.tokens._

import scala.language.implicitConversions
import scala.util.parsing.input.Position

class FormatterTest extends FunSuite {

  val cfg = Config()

  test("FormatterTest - apply method (1)") {
    val tokens =
      build(Newline("\n", _, _), Newline("\n", _, _), Newline("\n", _, _), Space(" ", _, _), Newline("\n", _, _), Literal("re", "re", _, _))
    val test     = Formatter(tokens)(using cfg)
    val expected = Seq(Newline("\n", 4, 5), Literal("re", "re", 5, 6))
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("FormatterTest - apply method (2)") {
    val tokens   = build(Literal("re", "re", _, _), Newline("\n", _, _), Space(" ", _, _), Space(" ", _, _))
    val test     = Formatter(tokens)(using cfg)
    val expected = Seq(Literal("re", "re", 0, 1))
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("FormatterTest - apply method (3): indentation error") {
    val tokens =
      build(Literal("re", "re", _, _), Newline("\n", _, _), Space(" ", _, _), Space(" ", _, _), Space(" ", _, _), Literal("with", "with", _, _))
    val test = Formatter(tokens)(using cfg)
    assert(test.isFailure)
  }

  test("FormatterTest - apply method (4): indentation") {
    val tokens = build(
      Literal("re", "re", _, _),
      Newline("\n", _, _),
      Space(" ", _, _),
      Space(" ", _, _),
      Space(" ", _, _),
      Space(" ", _, _),
      Literal("with", "with", _, _)
    )
    val test = Formatter(tokens)(using cfg)
    val expected = Seq(
      Literal("re", "re", 0, 1),
      Indent("", 1, 2),
      Literal("with", "with", 6, 7),
      Dedent("", 7, 7)
    )
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

}
