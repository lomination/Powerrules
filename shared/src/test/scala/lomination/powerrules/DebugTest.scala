package lomination.powerrules

import lomination.powerrules.FunSuite

// specific tests, can be ignored

class DebugTest extends FunSuite {

  given TmpTile = TmpTile(255)

  test("DebugTest (1): rotation of positions in shadow") {
    val m        = FullMatcher(Op.Is)
    val conds    = Seq(Pos.zero is m, Pos.n isnot m, Pos.e isnot m, Pos.s is m, Pos.w isnot m)
    val result   = conds.map(c => (c.pos.rotate(Dir.p3) is c.matcher))
    val expected = Seq(Pos.zero is m, Pos.w isnot m, Pos.n isnot m, Pos.e is m, Pos.s isnot m)
    assert(clue(result) == clue(expected))
  }

}
