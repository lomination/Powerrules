package lomination.ddnettools.writers

import munit.FunSuite
import lomination.ddnettools.*
import lomination.ddnettools.writers.BasicWriter.{given Writable[?]}

class ResetCommandWriter extends FunSuite {
  test("basic") {
    val struct   = Reset(Tile(0x12, Dir(Sign.+, Times.Zero)))
    val result   = struct.write
    val expected = "Index 18 NONE\n"
    assert(clue(result) == clue(expected))
  }
  test("random") {
    val struct   = Reset(Tile(0xa, Dir(Sign.+, Times.Zero)), Random(75))
    val result   = struct.write
    val expected = "Index 10 NONE\nRandom 75%\n"
    assert(clue(result) == clue(expected))
  }
  test("autorotate") {
    val struct   = Reset(Tile(0xa, Dir(Sign.+, Times.Zero)), autorotate = Seq(Dir.default, Dir(Sign.+, Times.One)))
    val result   = struct.write
    val expected = "Index 10 NONE\nIndex 10 ROTATE\n"
    assert(clue(result) == clue(expected))
  }
  // test("random & autorotate") {
  //   val struct   = Reset(Tile(0xa, Dir(Sign.+, Times.Zero)), Random(75), Seq(Dir.default, Dir(Sign.+, Times.One)))
  //   val result   = struct.write
  //   val expected = "Index 10 NONE\nRandom 75%\n"
  //   assert(clue(result) == clue(expected))
  // }
}

// class ReplaceCommandWriter extends FunSuite {
//   test("basic") {
//     val struct   = Replace(Tile(0x12, Dir(Sign.+, Times.Zero)), Seq(Cond(Pos(0, 0))))
//     val result   = struct.write
//     val expected = "Index 18 NONE\n"
//     assert(clue(result) == clue(expected))
//   }
//   test("random") {
//     val struct   = Replace(Tile(0xa, Dir(Sign.+, Times.Zero)), Random(75))
//     val result   = struct.write
//     val expected = "Index 10 NONE\nRandom 75%\n"
//     assert(clue(result) == clue(expected))
//   }
//   test("autorotate") {
//     val struct   = Replace(Tile(0xa, Dir(Sign.+, Times.Zero)), autorotate = Seq(Dir(Sign.+, Times.One)))
//     val result   = struct.write
//     val expected = "Index 10 NONE\nIndex 10 ROTATE\n"
//     assert(clue(result) == clue(expected))
//   }
//   test("random & autorotate") {
//     val struct   = Replace(Tile(0xa, Dir(Sign.+, Times.Zero)), Random(75))
//     val result   = struct.write
//     val expected = "Index 10 NONE\nRandom 75%\n"
//     assert(clue(result) == clue(expected))
//   }
// }
