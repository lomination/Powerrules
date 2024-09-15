package lomination.powerrules.ast

import lomination.powerrules.FunSuite
import lomination.powerrules.ast.{Dir, Pos}

class PosTest extends FunSuite {

  test("PosTest - rotate method (1)") {
    val test     = Pos.zero rotate Dir.m2
    val expected = Pos.zero
    assert(clue(test) == clue(expected))
  }

  test("PosTest - rotate method (2)") {
    val test     = Pos.n rotate Dir.p1
    val expected = Pos.e
    assert(clue(test) == clue(expected))
  }

  test("PosTest - rotate method (3)") {
    val test     = Pos.n rotate Dir.m1
    val expected = Pos.w
    assert(clue(test) == clue(expected))
  }

  test("PosTest - rotate method (4)") {
    val test     = Pos.n rotate Dir.p3
    val expected = Pos.w
    assert(clue(test) == clue(expected))
  }

  test("PosTest - rotate method (5)") {
    val test     = Pos(0, -3) rotate Dir.p2
    val expected = Pos(0, 3)
    assert(clue(test) == clue(expected))
  }

  test("PosTest - rotate method (6)") {
    val test     = Pos.ne rotate Dir.m1
    val expected = Pos.sw
    assert(clue(test) == clue(expected))
  }

}
