package lomination.powerrules

import munit.FunSuite

class PosTest extends FunSuite {
  test("pos: rotate combinason (1)") {
    val test     = Pos.zero rotate Dir.m2
    val expected = Pos.zero
    assert(clue(test) == clue(expected))
  }
  test("pos: rotate combinason (2)") {
    val test     = Pos.n rotate Dir.p1
    val expected = Pos.e
    assert(clue(test) == clue(expected))
  }
  test("pos: rotate combinason (3)") {
    val test     = Pos.n rotate Dir.m1
    val expected = Pos.w
    assert(clue(test) == clue(expected))
  }
  test("pos: rotate combinason (4)") {
    val test     = Pos.n rotate Dir.p3
    val expected = Pos.w
    assert(clue(test) == clue(expected))
  }
  test("pos: rotate combinason (5)") {
    val test     = Pos(0, -3) rotate Dir.p2
    val expected = Pos(0, 3)
    assert(clue(test) == clue(expected))
  }
  test("pos: rotate combinason (3)") {
    val test     = Pos.ne rotate Dir.m1
    val expected = Pos.sw
    assert(clue(test) == clue(expected))
  }
}
