package lomination.powerrules.macros

import lomination.powerrules.FunSuite
import lomination.powerrules.lexing.tokens.*
import scala.util.parsing.input.{NoPosition, Position}

class MacroApplierTest extends FunSuite {

  def build(tokens: (Position, Position) => Token*): Seq[Token] =
    tokens.map(_.apply(NoPosition, NoPosition)).toSeq

  test("MacroApplierTest - apply method (1)") {
    val tokens = build(Empty("empty", _, _), Space(" ", _, _), Dollar("$", _, _), Literal("m1", "m1", _, _), Space(" ", _, _), Empty("empty", _, _))
    val macros = Seq(
      Macro(
        Literal("m1", "m1", NoPosition, NoPosition),
        Seq(),
        build(Literal("macro", "macro", _, _), Space(" ", _, _), DecimalNumber(1, "1", _, _), Unknown("!", _, _))
      )
    )
    val test = MacroApplier(tokens, macros)
    val expected = build(
      Empty("empty", _, _),
      Space(" ", _, _),
      Literal("macro", "macro", _, _),
      Space(" ", _, _),
      DecimalNumber(1, "1", _, _),
      Unknown("!", _, _),
      Space(" ", _, _),
      Empty("empty", _, _)
    )
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("MacroApplierTest - apply method (2): with braces") {
    val tokens = build(
      Empty("empty", _, _),
      Space(" ", _, _),
      Dollar("$", _, _),
      LeftAcolade("{", _, _),
      Literal("m1", "m1", _, _),
      RightAcolade("}", _, _),
      Space(" ", _, _),
      Empty("empty", _, _)
    )
    val macros = Seq(
      Macro(
        Literal("m1", "m1", NoPosition, NoPosition),
        Seq(),
        build(Literal("macro", "macro", _, _), Space(" ", _, _), DecimalNumber(1, "1", _, _), Unknown("!", _, _))
      )
    )
    val test = MacroApplier(tokens, macros)
    val expected = build(
      Empty("empty", _, _),
      Space(" ", _, _),
      Literal("macro", "macro", _, _),
      Space(" ", _, _),
      DecimalNumber(1, "1", _, _),
      Unknown("!", _, _),
      Space(" ", _, _),
      Empty("empty", _, _)
    )
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("MacroApplierTest - apply method (3): nested macros") {
    val tokens = build(
      Empty("empty", _, _),
      Space(" ", _, _),
      Dollar("$", _, _),
      Literal("m1", "m1", _, _),
      LeftParenthese("(", _, _),
      Dollar("$", _, _),
      Literal("m2", "m2", _, _),
      RightParenthese(")", _, _),
      Space(" ", _, _),
      Empty("empty", _, _)
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
          Colon(":", _, _),
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
          Is("is", _, _),
          Space(" ", _, _),
          Literal("macro", "macro", _, _),
          Space(" ", _, _),
          DecimalNumber(2, "2", _, _)
        )
      )
    )
    val test = MacroApplier(tokens, macros)
    val expected = build(
      Empty("empty", _, _),
      Space(" ", _, _),
      Literal("macro", "macro", _, _),
      Space(" ", _, _),
      DecimalNumber(1, "1", _, _),
      Space(" ", _, _),
      Literal("contains", "contains", _, _),
      Colon(":", _, _),
      Space(" ", _, _),
      Literal("this", "this", _, _),
      Space(" ", _, _),
      Is("is", _, _),
      Space(" ", _, _),
      Literal("macro", "macro", _, _),
      Space(" ", _, _),
      DecimalNumber(2, "2", _, _),
      Space(" ", _, _),
      Empty("empty", _, _)
    )
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

}
