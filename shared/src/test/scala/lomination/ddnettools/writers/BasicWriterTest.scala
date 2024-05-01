package lomination.ddnettools.writers

import munit.FunSuite
import lomination.ddnettools.*
import lomination.ddnettools.writers.BasicWriter.{given Writable[?]}

class WriteReplace extends FunSuite {
  given DefaultTile = DefaultTile(255)
  test("basic replace") {
    val struct   = Replace(Seq(Tile(0x12)), Seq(Pos(0, 0) is FullMatcher))
    val result   = struct.write
    val expected = "Index 18 NONE\nNoDefaultRule\nPos 0 0 FULL\nNewRun\n"
    assert(clue(result) == clue(expected))
  }
  test("replace with random") {
    val struct   = Replace(Seq(Tile(0xa)), Seq(Pos(0, 0) is FullMatcher), random = Random(75))
    val result   = struct.write
    val expected = "Index 10 NONE\nNoDefaultRule\nPos 0 0 FULL\nRandom 75%\nNewRun\n"
    assert(clue(result) == clue(expected))
  }
  test("replace with autorotate") {
    val struct = Replace(Seq(Tile(0xa)), Seq(Pos(1, 0) is TileMatcher(2, Dir.p0)), rotations = Seq(Dir.p0, Dir.p1))
    val result = struct.write
    val expected = "Index 255 NONE\nNoDefaultRule\nPos 1 0 INDEX 2 NONE\nNewRun\n" +
      "Index 10 NONE\nPos 0 0 INDEX 255 NONE\nRandom 2\nNewRun\n" +
      "Index 10 ROTATE\nPos 0 0 INDEX 255 NONE\n" +
      "NewRun\n"
    assert(clue(result) == clue(expected))
  }
  test("replace with random & autorotate") {
    val struct = Replace(Seq(Tile(0xa)), Seq(Pos(1, 0) is TileMatcher(2, Dir.p0)), random = Random(50), rotations = Seq(Dir.p0, Dir.p1))
    val result = struct.write
    val expected = "Index 255 NONE\nNoDefaultRule\nPos 1 0 INDEX 2 NONE\nRandom 50%\nNewRun\n" +
      "Index 10 NONE\nPos 0 0 INDEX 255 NONE\nRandom 2\nNewRun\n" +
      "Index 10 ROTATE\nPos 0 0 INDEX 255 NONE\n" +
      "NewRun\n"
    assert(clue(result) == clue(expected))
  }
  test("replace with mutiple tiles") {
    val struct = Replace(Seq(Tile(1), Tile(2), Tile(3), Tile(4)), Seq(Pos(1, 0) is TileMatcher(0xa, Dir.p0)))
    val result = struct.write
    val expected = "Index 255 NONE\nNoDefaultRule\nPos 1 0 INDEX 10 NONE\nNewRun\n" +
      "Index 1 NONE\nPos 0 0 INDEX 255 NONE\nRandom 4\nNewRun\n" +
      "Index 2 NONE\nPos 0 0 INDEX 255 NONE\nRandom 3\nNewRun\n" +
      "Index 3 NONE\nPos 0 0 INDEX 255 NONE\nRandom 2\nNewRun\n" +
      "Index 4 NONE\nPos 0 0 INDEX 255 NONE\n" +
      "NewRun\n"
    assert(clue(result) == clue(expected))
  }
  test("replace with mutiple tiles and random") {
    val struct = Replace(Seq(Tile(1), Tile(2), Tile(3), Tile(4)), Seq(Pos(1, 0) is TileMatcher(0xa, Dir.p0)), random = Random(50f))
    val result = struct.write
    val expected = "Index 255 NONE\nNoDefaultRule\nPos 1 0 INDEX 10 NONE\nRandom 50%\nNewRun\n" +
      "Index 1 NONE\nPos 0 0 INDEX 255 NONE\nRandom 4\nNewRun\n" +
      "Index 2 NONE\nPos 0 0 INDEX 255 NONE\nRandom 3\nNewRun\n" +
      "Index 3 NONE\nPos 0 0 INDEX 255 NONE\nRandom 2\nNewRun\n" +
      "Index 4 NONE\nPos 0 0 INDEX 255 NONE\n" +
      "NewRun\n"
    assert(clue(result) == clue(expected))
  }
  test("replace with mutiple tiles, autorotate and random") {
    val struct = Replace(Seq(Tile(1), Tile(2)), Seq(Pos(1, 0) is TileMatcher(0xa, Dir.p0)), Random(50f), Seq(Dir.p0, Dir.p1))
    val result = struct.write
    val expected = "Index 255 NONE\nNoDefaultRule\nPos 1 0 INDEX 10 NONE\nRandom 50%\nNewRun\n" +
      "Index 1 NONE\nPos 0 0 INDEX 255 NONE\nRandom 4\nNewRun\n" +
      "Index 1 ROTATE\nPos 0 0 INDEX 255 NONE\nRandom 3\nNewRun\n" +
      "Index 2 NONE\nPos 0 0 INDEX 255 NONE\nRandom 2\nNewRun\n" +
      "Index 2 ROTATE\nPos 0 0 INDEX 255 NONE\n" +
      "NewRun\n"
    assert(clue(result) == clue(expected))
  }
}

// class WriteShadow extends FunSuite {
//   given DefaultTile = DefaultTile(255)
//   test("shadow") {
//     val struct   = Shadow((1 to 6).map(Tile(_)))
//     val result   = struct.write
//     val expected = "Index 255 NONE\nNoDefaultRule\nPos 0 0 FULL\nNewRun\n" +
//       "Index 1 NONE\nPos 0 0 INDEX 255 NONE\n" +
//       "Index 2 NONE\nPos 0 0 INDEX 255 NONE\nPos -1 0 INDEX 255 NONE\nPos 0 1 INDEX 255 NONE\nPos 1 0 INDEX 255 NONE\nPos 0 -1 INDEX 255 NONE\n" +
//       "Index 2 ROTATE\nPos 0 0 INDEX 255 NONE\nPos 0 1 INDEX 255 NONE\nPos 1 0 INDEX 255 NONE\nPos 0 -1 INDEX 255 NONE\nPos -1 0 INDEX 255 NONE\n" +
//       "Index 2 XFLIP YFLIP\nPos 0 0 INDEX 255 NONE\nPos 1 0 INDEX 255 NONE\nPos 0 -1 INDEX 255 NONE\nPos -1 0 INDEX 255 NONE\nPos 0 1 INDEX 255 NONE\n" +
//       "Index 2 XFLIP YFLIP ROTATE\nPos 0 0 INDEX 255 NONE\nPos 0 -1 INDEX 255 NONE\nPos -1 0 INDEX 255 NONE\nPos 0 1 INDEX 255 NONE\nPos 1 0 INDEX 255 NONE\n"
//     assert(clue(result) == clue(expected))
//   }
// }

class WriteComment extends FunSuite {
  given DefaultTile = DefaultTile(255)
  test("basic comment") {
    val struct   = Comment("# this is my comment")
    val result   = struct.write
    val expected = "# this is my comment\n"
    assert(clue(result) == clue(expected))
  }
  test("comment containing special characters") {
    val struct   = Comment("""# this is my comment: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~""")
    val result   = struct.write
    val expected = """# this is my comment: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~""" + "\n"
    assert(clue(result) == clue(expected))
  }
}
