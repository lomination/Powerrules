package lomination.ddnettools.writers

import munit.FunSuite
import lomination.ddnettools.*
import lomination.ddnettools.writers.BasicWriter.{given Writable[?]}

class ClearCommandWriter extends FunSuite {
  test("basic") {
    val struct   = Clear(Tile(0x12, Dir(Sign.Plus, Times.Zero)))
    val result   = struct.write
    val expected = "Index 18 NONE\nNoDefaultRule\n"
    assert(clue(result) == clue(expected))
  }
  test("random") {
    val struct   = Clear(Tile(0xa, Dir(Sign.Plus, Times.Zero)), Random(0.75))
    val result   = struct.write
    val expected = "Index 10 NONE\nNoDefaultRule\nRandom 75%\n"
    assert(clue(result) == clue(expected))
  }
  test("autorotate") {
    val struct   = Clear(Tile(0xa, Dir(Sign.Plus, Times.Zero)), autorotate = Seq(Dir(Sign.Plus, Times.One)))
    val result   = struct.write
    val expected = "Index 10 NONE\nNoDefaultRule\nIndex 10 ROTATE\nNoDefaultRule\n"
    assert(clue(result) == clue(expected))
  }
  test("random & autorotate") {
    val struct   = Clear(Tile(0xa, Dir(Sign.Plus, Times.Zero)), Random(0.75))
    val result   = struct.write
    val expected = "Index 10\nNoDefaultRule\nRandom 0.75"
    assert(clue(result) == clue(expected))
  }
}
