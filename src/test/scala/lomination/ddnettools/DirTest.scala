package lomination.ddnettools

import munit.FunSuite

class DirTest extends FunSuite {
  test("rotate combinason (1)") {
    val test     = Dir(Sign.+, Times.One) rotate Dir(Sign.-, Times.One)
    val expected = Dir(Sign.-, Times.Two)
    assert(clue(test) == clue(expected))
  }
  test("rotate combinason (2)") {
    val test     = Dir(Sign.+, Times.One) rotate Dir(Sign.-, Times.Two)
    val expected = Dir(Sign.-, Times.Three)
    assert(clue(test) == clue(expected))
  }
  test("rotate combinason (3)") {
    val test     = Dir(Sign.-, Times.Two) rotate Dir(Sign.-, Times.One)
    val expected = Dir(Sign.+, Times.Three)
    assert(clue(test) == clue(expected))
  }
  test("rotate with default dir (1)") {
    val test     = Dir.default rotate Dir(Sign.-, Times.Three)
    val expected = Dir(Sign.-, Times.Three)
    assert(clue(test) == clue(expected))
  }
  test("rotate with default dir (2)") {
    val test     = Dir(Sign.+, Times.Two) rotate Dir.default
    val expected = Dir(Sign.+, Times.Two)
    assert(clue(test) == clue(expected))
  }
  test("rotate commutativity") {
    val test     = Dir(Sign.-, Times.Three) rotate Dir(Sign.+, Times.Two)
    val expected = Dir(Sign.+, Times.Two) rotate Dir(Sign.-, Times.Three)
    assert(clue(test) == clue(expected))
  }
  test("second constructor") {
    val test     = new Dir(-1, 0)
    val expected = Dir(Sign.-, Times.Zero)
    assert(clue(test) == clue(expected))
  }
}
