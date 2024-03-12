package lomination.ddnettools.parser

import munit.FunSuite
import lomination.ddnettools.*

class ParseReset extends FunSuite {
  test("reset") {
    val input  = "reset\n  tile 2e-2\nendreset"
    val parser = MyParser()
    val result = parser.parse(parser.command, input)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(Reset(Tile(0x2e, Dir(Sign.-, Times.Two)))))
  }
  test("indented reset") {
    val input  = "reset\n  tile\n    2e-2\nendreset"
    val parser = MyParser()
    val result = parser.parse(parser.command, input)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(Reset(Tile(0x2e, Dir(Sign.-, Times.Two)))))
  }
  test("nodefrule reset") {
    val input  = "reset\n  tile 2e-2\n  nodefaultrule\nendreset"
    val parser = MyParser()
    val result = parser.parse(parser.command, input)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(Reset(Tile(0x2e, Dir(Sign.-, Times.Two)), noDefRule = true)))
  }
  test("indented nodefrule reset") {
    val input  = "reset\n  tile\n    2e-2\n  nodefaultrule\nendreset"
    val parser = MyParser()
    val result = parser.parse(parser.command, input)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(Reset(Tile(0x2e, Dir(Sign.-, Times.Two)), noDefRule = true)))
  }
}

class ParseReplace extends FunSuite {
  test("replace") {
    val input  = "replace\n  tile 1f-0\n  if (0,0) is full\nendreplace"
    val parser = MyParser()
    val result = parser.parse(parser.command, input)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(Replace(Tile(0x1f, Dir(Sign.-, Times.Zero)), Seq(Cond(Pos(0, 0), Operator.Equal, TileMatcher(FullTile, AnyDir))))))
  }
  test("indented replace") {
    val input  = "replace\n  tile\n    1f-0\n  if\n    (0,0) is full\nendreplace"
    val parser = MyParser()
    val result = parser.parse(parser.command, input)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(Replace(Tile(0x1f, Dir(Sign.-, Times.Zero)), Seq(Cond(Pos(0, 0), Operator.Equal, TileMatcher(FullTile, AnyDir))))))
  }
  test("nodefrule replace") {
    val input  = "replace\n  tile 1f-0\n  if (0,0) is full\n  nodefaultrule\nendreplace"
    val parser = MyParser()
    val result = parser.parse(parser.command, input)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(Replace(Tile(0x1f, Dir(Sign.-, Times.Zero)), Seq(Cond(Pos(0, 0), Operator.Equal, TileMatcher(FullTile, AnyDir))), noDefRule = true)))
  }
  test("indented nodefrule replace") {
    val input  = "replace\n  tile\n    1f-0\n  if\n    (0,0) is full\n  nodefaultrule\nendreplace"
    val parser = MyParser()
    val result = parser.parse(parser.command, input)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(Replace(Tile(0x1f, Dir(Sign.-, Times.Zero)), Seq(Cond(Pos(0, 0), Operator.Equal, TileMatcher(FullTile, AnyDir))), noDefRule = true)))
  }
}

class ParseTile extends FunSuite {
  test("basic") {
    val input    = "02+0"
    val parser   = MyParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0x02, Dir(Sign.+, Times.Zero))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("1 digit") {
    val input    = "3+0"
    val parser   = MyParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0x3, Dir(Sign.+, Times.Zero))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("hexa") {
    val input    = "0a+0"
    val parser   = MyParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0x0a, Dir(Sign.+, Times.Zero))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("1 digit hexa") {
    val input    = "b+0"
    val parser   = MyParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0xb, Dir(Sign.+, Times.Zero))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("positive dir") {
    val input    = "a2+2"
    val parser   = MyParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0xa2, Dir(Sign.+, Times.Two))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("negative dir") {
    val input    = "b3-3"
    val parser   = MyParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0xb3, Dir(Sign.-, Times.Three))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
}
