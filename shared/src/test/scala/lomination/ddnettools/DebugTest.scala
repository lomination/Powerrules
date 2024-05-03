package lomination.ddnettools

import munit.FunSuite

// specific tests, can be ignored

class DebugTest extends FunSuite {
  given DefaultTile = DefaultTile(255)
  test("wrong rotate of pos in conds while writing shadow") {
    val m        = FullMatcher(Op.Is)
    val conds    = Seq(Pos.zero is m, Pos.n isnot m, Pos.e isnot m, Pos.s is m, Pos.w isnot m)
    val t        = conds.map(c => (c.pos.rotate(new Dir(1, 3)) is c.matcher))
    val expected = Seq(Pos.zero is m, Pos.w isnot m, Pos.n isnot m, Pos.e is m, Pos.s isnot m)
    assert(clue(t) == clue(expected))
  }
}
