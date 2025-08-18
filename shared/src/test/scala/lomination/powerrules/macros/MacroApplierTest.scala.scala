package lomination.powerrules.macros

import lomination.powerrules.FunSuite
import lomination.powerrules.Functions.build
import lomination.powerrules.Functions.given
import lomination.powerrules.lexing.tokens._

import scala.language.implicitConversions
import scala.util.parsing.input.{NoPosition, Position}

class MacroApplierTest extends FunSuite {

  test("MacroApplierTest - apply method (1)") {
    val tokens = build(
      Literal("empty", "empty", _, _),
      Space(" ", _, _),
      Dollar("$", _, _),
      Literal("m1", "m1", _, _),
      Space(" ", _, _),
      Literal("empty", "empty", _, _)
    )
    val macros = Seq(
      Macro(
        Literal("m1", "m1", NoPosition, NoPosition),
        Seq(),
        build(Literal("macro", "macro", _, _), Space(" ", _, _), DecimalNumber(1, "1", _, _), Unknown("!", _, _))
      )
    )
    val test = MacroApplier(tokens, macros)
    val expected = Seq(
      Literal("empty", "empty", 0, 1),
      Space(" ", 1, 2),
      Literal("macro", "macro", 2, 3),
      Space(" ", 2, 3),
      DecimalNumber(1, "1", 2, 3),
      Unknown("!", 2, 3),
      Space(" ", 4, 5),
      Literal("empty", "empty", 5, 6)
    )
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("MacroApplierTest - apply method (2): with braces") {
    val tokens = build(
      Literal("empty", "empty", _, _),
      Space(" ", _, _),
      Dollar("$", _, _),
      LeftAcolade("{", _, _),
      Literal("m1", "m1", _, _),
      RightAcolade("}", _, _),
      Space(" ", _, _),
      Literal("empty", "empty", _, _)
    )
    val macros = Seq(
      Macro(
        Literal("m1", "m1", NoPosition, NoPosition),
        Seq(),
        build(Literal("macro", "macro", _, _), Space(" ", _, _), DecimalNumber(1, "1", _, _), Unknown("!", _, _))
      )
    )
    val test = MacroApplier(tokens, macros)
    val expected = Seq(
      Literal("empty", "empty", 0, 1),
      Space(" ", 1, 2),
      Literal("macro", "macro", 2, 3),
      Space(" ", 2, 3),
      DecimalNumber(1, "1", 2, 3),
      Unknown("!", 2, 3),
      Space(" ", 6, 7),
      Literal("empty", "empty", 7, 8)
    )
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("MacroApplierTest - apply method (3): nested macros") {
    val tokens = build(
      Literal("empty", "empty", _, _),
      Space(" ", _, _),
      Dollar("$", _, _),
      Literal("m1", "m1", _, _),
      LeftParenthese("(", _, _),
      Dollar("$", _, _),
      Literal("m2", "m2", _, _),
      RightParenthese(")", _, _),
      Space(" ", _, _),
      Literal("empty", "empty", _, _)
    )
    val macros = Seq(
      Macro(
        Literal("m1", "m1", NoPosition, NoPosition),
        Seq("content"),
        build(
          Literal("macro", "macro", _, _),
          Space(" ", _, _),
          DecimalNumber(1, "1", _, _),
          Space(" ", _, _),
          Literal("contains", "contains", _, _),
          Unknown(":", _, _),
          Space(" ", _, _),
          LeftChevron("<", _, _),
          Literal("content", "content", _, _),
          RightChevron(">", _, _)
        )
      ),
      Macro(
        Literal("m2", "m2", NoPosition, NoPosition),
        Seq(),
        build(
          Literal("this", "this", _, _),
          Space(" ", _, _),
          Literal("is", "is", _, _),
          Space(" ", _, _),
          Literal("macro", "macro", _, _),
          Space(" ", _, _),
          DecimalNumber(2, "2", _, _)
        )
      )
    )
    val test = MacroApplier(tokens, macros)
    val expected = Seq(
      Literal("empty", "empty", 0, 1),
      Space(" ", 1, 2),
      Literal("macro", "macro", 2, 3),
      Space(" ", 2, 3),
      DecimalNumber(1, "1", 2, 3),
      Space(" ", 2, 3),
      Literal("contains", "contains", 2, 3),
      Unknown(":", 2, 3),
      Space(" ", 2, 3),
      Literal("this", "this", 5, 6),
      Space(" ", 5, 6),
      Literal("is", "is", 5, 6),
      Space(" ", 5, 6),
      Literal("macro", "macro", 5, 6),
      Space(" ", 5, 6),
      DecimalNumber(2, "2", 5, 6),
      Space(" ", 8, 9),
      Literal("empty", "empty", 9, 10)
    )
    assert(test.isSuccess)
    // assert(test.get.map(_.raw).reduce((s1, s2) => s1 + s2) == expected.map(_.raw).reduce((s1, s2) => s1 + s2))
    assert(clue(test.get) == clue(expected))
  }

}
