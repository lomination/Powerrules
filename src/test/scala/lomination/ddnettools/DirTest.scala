package lomination.ddnettools

import munit.FunSuite

class DirTest extends FunSuite {
  test("combinason (1)") {
    val test     = Dir(Sign.Plus, Times.One) & Dir(Sign.Minus, Times.One)
    val expected = Dir(Sign.Minus, Times.Two)
    assert(clue(test) == clue(expected))
  }
  test("combinason (2)") {
    val test     = Dir(Sign.Plus, Times.One) & Dir(Sign.Minus, Times.Two)
    val expected = Dir(Sign.Minus, Times.Three)
    assert(clue(test) == clue(expected))
  }
  test("combinason (3)") {
    val test     = Dir(Sign.Minus, Times.Two) & Dir(Sign.Minus, Times.One)
    val expected = Dir(Sign.Plus, Times.Three)
    assert(clue(test) == clue(expected))
  }
  test("commutativity") {
    val test     = Dir(Sign.Minus, Times.Three) & Dir(Sign.Plus, Times.Two)
    val expected = Dir(Sign.Plus, Times.Two) & Dir(Sign.Minus, Times.Three)
    assert(clue(test) == clue(expected))
  }
}
