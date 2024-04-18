package lomination.ddnettools.parser

import munit.FunSuite

class MacroTest extends FunSuite {
  test("replace with values") {
    val m        = Macro("test", Seq("tile", "cond"), "re with <tile> if <cond>")
    val result   = m(Seq("1+0", "0 0 is full"))
    val expected = "re with 1+0 if 0 0 is full"
    assert(result.isSuccess)
    assert(clue(result.get) == clue(expected))
  }
  test("replace with values (1)") {
    val m        = Macro("reset", Seq("tile"), "re with <tile> if 0 0 is full")
    val result   = m(Seq("12-2"))
    val expected = "re with 12-2 if 0 0 is full"
    assert(result.isSuccess)
    assert(clue(result.get) == clue(expected))
  }
  test("replace (params > values)") {
    val m        = Macro("test", Seq("tile", "cond"), "re with <tile> if <cond>")
    val result   = m(Seq("1+0"))
    val expected = "re with 1+0 if "
    assert(result.isFailure)
  }
  test("replace (params < values)") {
    val m        = Macro("test", Seq("tile", "cond"), "re with <tile> if <cond>")
    val result   = m(Seq("1+0", "0 0 is full", "0 1 is 1+0"))
    val expected = "re with 1+0 if 0 0 is full"
    assert(result.isFailure)
  }
}
