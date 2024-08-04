package lomination.powerrules

import lomination.powerrules.FunSuite

class DirRotate extends FunSuite {

  test("DirRotate (1)") {
    val test     = Dir.p1 rotate Dir.m1
    val expected = Dir.m2
    assert(clue(test) == clue(expected))
  }

  test("DirRotate (2)") {
    val test     = Dir.p1 rotate Dir.m2
    val expected = Dir.m3
    assert(clue(test) == clue(expected))
  }

  test("DirRotate (3)") {
    val test     = Dir.m2 rotate Dir.m1
    val expected = Dir.p3
    assert(clue(test) == clue(expected))
  }

  test("DirRotate (4): with default dir") {
    val test     = Dir.p0 rotate Dir.m3
    val expected = Dir.m3
    assert(clue(test) == clue(expected))
  }

  test("DirRotate (5): with default dir") {
    val test     = Dir.p2 rotate Dir.p0
    val expected = Dir.p2
    assert(clue(test) == clue(expected))
  }

  test("DirRotate (6): commutativity") {
    val test     = Dir.m3 rotate Dir.p2
    val expected = Dir.p2 rotate Dir.m3
    assert(clue(test) == clue(expected))
  }

  test("DirRotate (7): associativity") {
    val test     = (Dir.m1 rotate Dir.m2) rotate Dir.p1
    val expected = Dir.m1 rotate (Dir.m2 rotate Dir.p1)
    assert(clue(test) == clue(expected))
  }

}
