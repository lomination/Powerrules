package lomination.powerrules

import munit.FunSuite

class DirTest extends FunSuite {
  test("dir: rotate combinason (1)") {
    val test     = Dir.p1 rotate Dir.m1
    val expected = Dir.m2
    assert(clue(test) == clue(expected))
  }
  test("dir: rotate combinason (2)") {
    val test     = Dir.p1 rotate Dir.m2
    val expected = Dir.m3
    assert(clue(test) == clue(expected))
  }
  test("dir: rotate combinason (3)") {
    val test     = Dir.m2 rotate Dir.m1
    val expected = Dir.p3
    assert(clue(test) == clue(expected))
  }
  test("dir: rotate with default dir (1)") {
    val test     = Dir.p0 rotate Dir.m3
    val expected = Dir.m3
    assert(clue(test) == clue(expected))
  }
  test("dir: rotate with default dir (2)") {
    val test     = Dir.p2 rotate Dir.p0
    val expected = Dir.p2
    assert(clue(test) == clue(expected))
  }
  test("dir: rotate commutativity") {
    val test     = Dir.m3 rotate Dir.p2
    val expected = Dir.p2 rotate Dir.m3
    assert(clue(test) == clue(expected))
  }
  test("dir: second constructor") {
    val test     = new Dir(-1, 0)
    val expected = Dir.m0
    assert(clue(test) == clue(expected))
  }
}
