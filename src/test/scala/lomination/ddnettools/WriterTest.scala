package lomination.ddnettools.writers

import munit.FunSuite
import lomination.ddnettools.*
import lomination.ddnettools.writers.BasicWriter.*
// import lomination.ddnettools.writers.BasicWriter.given_Writable_Clear

class ClearCommandWriter extends FunSuite {
  test("basic") {
    val struct   = Clear(Tile(0x12, Dir(Sign.Plus, Times.Zero)))
    val result   = struct.write
    val expected = "Index 18\nNoDefaultRule"
    assert(result == expected)
  }
  test("random") {
    val struct   = Clear(Tile(0xa, Dir(Sign.Plus, Times.Zero)), 0.75)
    val result   = struct.write
    val expected = "Index 10\nNoDefaultRule\nRandom 0.75"
    assert(result == expected)
  }
  test("autorotate") {
    val struct   = Clear(Tile(0xa, Dir(Sign.Plus, Times.Zero)), autorotate = Seq(Dir(Sign.Plus, Times.Zero)))
    val result   = struct.write
    val expected = "Index 10\nNoDefaultRule\nRandom 0.75"
    assert(result == expected)
  }
  test("random & autorotate") {
    val struct   = Clear(Tile(0xa, Dir(Sign.Plus, Times.Zero)), 0.75)
    val result   = struct.write
    val expected = "Index 10\nNoDefaultRule\nRandom 0.75"
    assert(result == expected)
  }
}
