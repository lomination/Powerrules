package lomination.powerrules.macros

import lomination.powerrules.FunSuite
import lomination.powerrules.lexing.tokens.*
import scala.util.parsing.input.{Position, NoPosition}

class MacroApplierTest extends FunSuite {

  def build(tokens: (Position, Position) => Token*): Seq[Token] =
    tokens.map(_.apply(NoPosition, NoPosition)).toSeq

  test("MacroApplierTest - apply method (1)") {
    val tokens   = build(Empty("empty", _, _), Space(" ", _, _), Dollar("$", _, _), Literal("m1", "m1", _, _), Space(" ", _, _), Empty("empty", _, _))
    val content  = build(Literal("macro", "macro", _, _), Space(" ", _, _), DecimalNumber(1, "1", _, _), Unknown("!", _, _))
    val macros   = Seq(Macro(Literal("m1", "m1", NoPosition, NoPosition), Seq(), content))
    val test     = MacroApplier(tokens, macros)
    val expected = build(Empty("empty", _, _), Space(" ", _, _), Literal("macro", "macro", _, _), Space(" ", _, _), DecimalNumber(1, "1", _, _), Unknown("!", _, _), Space(" ", _, _), Empty("empty", _, _))
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

}
