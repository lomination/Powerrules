package lomination.powerrules.parser

import munit.FunSuite

class MacroTest extends FunSuite {

  test("MacroTest (1)") {
    val m        = Macro("test", Seq("tile", "cond"), "re with <tile> if <cond>")
    val result   = m(Seq("1+0", "0 0 is full"))
    val expected = "re with 1+0 if 0 0 is full"
    assert(result.isSuccess)
    assert(clue(result.get) == clue(expected))
  }

  test("MacroTest (2)") {
    val m        = Macro("reset", Seq("tile"), "re with <tile> if 0 0 is full")
    val result   = m(Seq("12-2"))
    val expected = "re with 12-2 if 0 0 is full"
    assert(result.isSuccess)
    assert(clue(result.get) == clue(expected))
  }

  test("MacroTest (3): not enough params") {
    val m      = Macro("test", Seq("tile", "cond"), "re with <tile> if <cond>")
    val result = m(Seq("1+0"))
    assert(result.isFailure)
  }

  test("MacroTest (1): too many params") {
    val m      = Macro("test", Seq("tile", "cond"), "re with <tile> if <cond>")
    val result = m(Seq("1+0", "0 0 is full", "0 1 is 1+0"))
    assert(result.isFailure)
  }

}
