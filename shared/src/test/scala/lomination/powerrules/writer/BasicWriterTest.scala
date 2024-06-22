package lomination.powerrules.writer

import munit.FunSuite
import lomination.powerrules.{Comment,Dir,FullMatcher,Op,Pos,Replace,Random,Shadow,Tile,TileMatcher,TmpTile}
import lomination.powerrules.writer.BasicWriter.{given Writable[?]}

class WriteReplace extends FunSuite {

  given TmpTile = TmpTile(255)

  test("WriteReplace (1)") {
    val struct = Replace(Seq(Tile(0x12)), Seq(Pos(0, 0) is FullMatcher(Op.Is)))
    val result = struct.write
    val expected = """|Index 18 NONE
                      |NoDefaultRule
                      |Pos 0 0 FULL
                      |NewRun
                      |""".stripMargin
    assert(clue(struct.write) == clue(expected))
  }

  test("WriteReplace (2): with random") {
    val struct = Replace(Seq(Tile(0xa)), Seq(Pos(0, 0) is FullMatcher(Op.Is)), random = Random(75))
    val result = struct.write
    val expected = """|Index 10 NONE
                      |NoDefaultRule
                      |Pos 0 0 FULL
                      |Random 75%
                      |NewRun
                      |""".stripMargin
    assert(clue(struct.write) == clue(expected))
  }

  test("WriteReplace (3): with autorotate") {
    val struct = Replace(Seq(Tile(0xa)), Seq(Pos(1, 0) is TileMatcher(2, Dir.p0)), rotations = Seq(Dir.p0, Dir.p1))
    val expected = """|Index 255 NONE
                      |NoDefaultRule
                      |Pos 1 0 INDEX 2 NONE
                      |NewRun
                      |Index 10 NONE
                      |Pos 0 0 INDEX 255 NONE
                      |Random 2
                      |NewRun
                      |Index 10 ROTATE
                      |Pos 0 0 INDEX 255 NONE
                      |NewRun
                      |""".stripMargin
    assert(clue(struct.write) == clue(expected))
  }

  test("WriteReplace (4): with random and autorotate") {
    val struct = Replace(Seq(Tile(0xa)), Seq(Pos(1, 0) is TileMatcher(2, Dir.p0)), random = Random(50), rotations = Seq(Dir.p0, Dir.p1))
    val expected = """|Index 255 NONE
                      |NoDefaultRule
                      |Pos 1 0 INDEX 2 NONE
                      |Random 50%
                      |NewRun
                      |Index 10 NONE
                      |Pos 0 0 INDEX 255 NONE
                      |Random 2
                      |NewRun
                      |Index 10 ROTATE
                      |Pos 0 0 INDEX 255 NONE
                      |NewRun
                      |""".stripMargin
    assert(clue(struct.write) == clue(expected))
  }

  test("WriteReplace (5): with mutiple tiles") {
    val struct = Replace(Seq(Tile(1), Tile(2), Tile(3), Tile(4)), Seq(Pos(1, 0) is TileMatcher(0xa, Dir.p0)))
    val expected = """|Index 255 NONE
                      |NoDefaultRule
                      |Pos 1 0 INDEX 10 NONE
                      |NewRun
                      |Index 1 NONE
                      |Pos 0 0 INDEX 255 NONE
                      |Random 4
                      |NewRun
                      |Index 2 NONE
                      |Pos 0 0 INDEX 255 NONE
                      |Random 3
                      |NewRun
                      |Index 3 NONE
                      |Pos 0 0 INDEX 255 NONE
                      |Random 2
                      |NewRun
                      |Index 4 NONE
                      |Pos 0 0 INDEX 255 NONE
                      |NewRun
                      |""".stripMargin
    assert(clue(struct.write) == clue(expected))
  }

  test("WriteReplace (6): with mutiple tiles and random") {
    val struct = Replace(Seq(Tile(1), Tile(2), Tile(3), Tile(4)), Seq(Pos(1, 0) is TileMatcher(0xa, Dir.p0)), random = Random(50f))
    val expected = """|Index 255 NONE
                      |NoDefaultRule
                      |Pos 1 0 INDEX 10 NONE
                      |Random 50%
                      |NewRun
                      |Index 1 NONE
                      |Pos 0 0 INDEX 255 NONE
                      |Random 4
                      |NewRun
                      |Index 2 NONE
                      |Pos 0 0 INDEX 255 NONE
                      |Random 3
                      |NewRun
                      |Index 3 NONE
                      |Pos 0 0 INDEX 255 NONE
                      |Random 2
                      |NewRun
                      |Index 4 NONE
                      |Pos 0 0 INDEX 255 NONE
                      |NewRun
                      |""".stripMargin
    assert(clue(struct.write) == clue(expected))
  }

  test("WriteReplace (7): with mutiple tiles, autorotate and random") {
    val struct = Replace(Seq(Tile(1), Tile(2)), Seq(Pos(1, 0) is TileMatcher(0xa, Dir.p0)), Random(50), Seq(Dir.p0, Dir.p1))
    val expected = """|Index 255 NONE
                      |NoDefaultRule
                      |Pos 1 0 INDEX 10 NONE
                      |Random 50%
                      |NewRun
                      |Index 1 NONE
                      |Pos 0 0 INDEX 255 NONE
                      |Random 4
                      |NewRun
                      |Index 1 ROTATE
                      |Pos 0 0 INDEX 255 NONE
                      |Random 3
                      |NewRun
                      |Index 2 NONE
                      |Pos 0 0 INDEX 255 NONE
                      |Random 2
                      |NewRun
                      |Index 2 ROTATE
                      |Pos 0 0 INDEX 255 NONE
                      |NewRun
                      |""".stripMargin
    assert(clue(struct.write) == clue(expected))
  }

}

// class WriteShadow extends FunSuite {
//   given TmpTile = TmpTile(255)
//   test("shadow") {
//     val struct   = Shadow((1 to 6).map(Tile(_)))
//     val expected = "Index 255 NONE\nNoDefaultRule\nPos 0 0 FULL\nNewRun\n" +
//       "Index 1 NONE\nPos 0 0 INDEX 255 NONE\n" +
//       "Index 2 NONE\nPos 0 0 INDEX 255 NONE\nPos -1 0 INDEX 255 NONE\nPos 0 1 INDEX 255 NONE\nPos 1 0 INDEX 255 NONE\nPos 0 -1 INDEX 255 NONE\n" +
//       "Index 2 ROTATE\nPos 0 0 INDEX 255 NONE\nPos 0 1 INDEX 255 NONE\nPos 1 0 INDEX 255 NONE\nPos 0 -1 INDEX 255 NONE\nPos -1 0 INDEX 255 NONE\n" +
//       "Index 2 XFLIP YFLIP\nPos 0 0 INDEX 255 NONE\nPos 1 0 INDEX 255 NONE\nPos 0 -1 INDEX 255 NONE\nPos -1 0 INDEX 255 NONE\nPos 0 1 INDEX 255 NONE\n" +
//       "Index 2 XFLIP YFLIP ROTATE\nPos 0 0 INDEX 255 NONE\nPos 0 -1 INDEX 255 NONE\nPos -1 0 INDEX 255 NONE\nPos 0 1 INDEX 255 NONE\nPos 1 0 INDEX 255 NONE\n"
//     assert(clue(struct.write) == clue(expected))
//   }
// }

class WriteComment extends FunSuite {

  given TmpTile = TmpTile(255)

  test("WriteComment (1)") {
    val struct = Comment("""# comment content""")
    val result = struct.write
    val expected = """|# comment content
                      |""".stripMargin
    assert(clue(struct.write) == clue(expected))
  }

  test("WriteComment (2): with special characters") {
    val struct = Comment("""# comment content: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~""")
    val result = struct.write
    val expected = """|# comment content: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
                      |""".stripMargin
    assert(clue(struct.write) == clue(expected))
  }

  test("WriteComment (3): with trailing spaces") {
    val struct = Comment("""# comment content     """)
    val result = struct.write
    val expected = """|# comment content
                      |""".stripMargin
    assert(clue(struct.write) == clue(expected))
  }

}
