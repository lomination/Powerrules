package lomination.powerrules.macros

import lomination.powerrules.FunSuite
import lomination.powerrules.lexing.tokens.*
import scala.util.parsing.input.{NoPosition, Position}
import lomination.powerrules.TestPos
import lomination.powerrules.Functions.build

class MacroApplierTest extends FunSuite {

  test("MacroApplierTest - apply method (1)") {
    val tokens = build(Literal("empty", "empty", _, _), Space(" ", _, _), Dollar("$", _, _), Literal("m1", "m1", _, _), Space(" ", _, _), Literal("empty", "empty", _, _))
    val macros = Seq(
      Macro(
        Literal("m1", "m1", NoPosition, NoPosition),
        Seq(),
        build(Literal("macro", "macro", _, _), Space(" ", _, _), DecimalNumber(1, "1", _, _), Unknown("!", _, _))
      )
    )
    val test = MacroApplier(tokens, macros)
    val expected = Seq(
      Literal("empty", "empty", TestPos(0), TestPos(1)),
      Space(" ", TestPos(1), TestPos(2)),
      Literal("macro", "macro", TestPos(2), TestPos(3)),
      Space(" ", TestPos(2), TestPos(3)),
      DecimalNumber(1, "1", TestPos(2), TestPos(3)),
      Unknown("!", TestPos(2), TestPos(3)),
      Space(" ", TestPos(4), TestPos(5)),
      Literal("empty", "empty", TestPos(5), TestPos(6))
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
      Literal("empty", "empty", TestPos(0), TestPos(1)),
      Space(" ", TestPos(1), TestPos(2)),
      Literal("macro", "macro", TestPos(2), TestPos(3)),
      Space(" ", TestPos(2), TestPos(3)),
      DecimalNumber(1, "1", TestPos(2), TestPos(3)),
      Unknown("!", TestPos(2), TestPos(3)),
      Space(" ", TestPos(6), TestPos(7)),
      Literal("empty", "empty", TestPos(7), TestPos(8))
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
      Literal("empty", "empty", TestPos(0), TestPos(1)),
      Space(" ", TestPos(1), TestPos(2)),
      Literal("macro", "macro", TestPos(2), TestPos(3)),
      Space(" ", TestPos(2), TestPos(3)),
      DecimalNumber(1, "1", TestPos(2), TestPos(3)),
      Space(" ", TestPos(2), TestPos(3)),
      Literal("contains", "contains", TestPos(2), TestPos(3)),
      Unknown(":", TestPos(2), TestPos(3)),
      Space(" ", TestPos(2), TestPos(3)),
      Literal("this", "this", TestPos(5), TestPos(6)),
      Space(" ", TestPos(5), TestPos(6)),
      Literal("is", "is", TestPos(5), TestPos(6)),
      Space(" ", TestPos(5), TestPos(6)),
      Literal("macro", "macro", TestPos(5), TestPos(6)),
      Space(" ", TestPos(5), TestPos(6)),
      DecimalNumber(2, "2", TestPos(5), TestPos(6)),
      Space(" ", TestPos(8), TestPos(9)),
      Literal("empty", "empty", TestPos(9), TestPos(10))
    )
    assert(test.isSuccess)
    // assert(test.get.map(_.raw).reduce((s1, s2) => s1 + s2) == expected.map(_.raw).reduce((s1, s2) => s1 + s2))
    assert(clue(test.get) == clue(expected))
  }

}
