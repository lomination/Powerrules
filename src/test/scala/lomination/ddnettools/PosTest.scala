package lomination.ddnettools

import munit.FunSuite

class PosTest extends FunSuite {
  test("pos: rotate combinason (1)") {
    val test     = Pos.zero rotate new Dir(-1, 2)
    val expected = Pos.zero
    assert(clue(test) == clue(expected))
  }
  test("pos: rotate combinason (2)") {
    val test     = Pos.n rotate new Dir(+1, 1)
    val expected = Pos.e
    assert(clue(test) == clue(expected))
  }
  test("pos: rotate combinason (3)") {
    val test     = Pos.n rotate new Dir(-1, 1)
    val expected = Pos.w
    assert(clue(test) == clue(expected))
  }
  test("pos: rotate combinason (4)") {
    val test     = Pos.n rotate new Dir(+1, 3)
    val expected = Pos.w
    assert(clue(test) == clue(expected))
  }
  test("pos: rotate combinason (5)") {
    val test     = Pos(0, -3) rotate new Dir(+1, 2)
    val expected = Pos(0, 3)
    assert(clue(test) == clue(expected))
  }
}
