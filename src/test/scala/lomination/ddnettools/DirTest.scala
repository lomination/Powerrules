package lomination.ddnettools

import munit.FunSuite

class DirTest extends FunSuite {
  test("combinason (1)") {
    val test     = Dir(Sign.+, Times.One) & Dir(Sign.-, Times.One)
    val expected = Dir(Sign.-, Times.Two)
    assert(clue(test) == clue(expected))
  }
  test("combinason (2)") {
    val test     = Dir(Sign.+, Times.One) & Dir(Sign.-, Times.Two)
    val expected = Dir(Sign.-, Times.Three)
    assert(clue(test) == clue(expected))
  }
  test("combinason (3)") {
    val test     = Dir(Sign.-, Times.Two) & Dir(Sign.-, Times.One)
    val expected = Dir(Sign.+, Times.Three)
    assert(clue(test) == clue(expected))
  }
  test("commutativity") {
    val test     = Dir(Sign.-, Times.Three) & Dir(Sign.+, Times.Two)
    val expected = Dir(Sign.+, Times.Two) & Dir(Sign.-, Times.Three)
    assert(clue(test) == clue(expected))
  }
}
