package lomination.ddnettools.parser

import munit.FunSuite
import lomination.ddnettools.*
import scala.language.experimental

class ParseReplace extends FunSuite {
  test("replace") {
    val input  = "replace\n  tile 2e-2\nendreplace"
    val parser = MyParser()
    val result = parser.parse(parser.command, input)
    val expected = Replace(Tile(0x2e, Dir(Sign.-, Times.Two)))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("indented replace") {
    val input  = "replace\n  tile\n    2e-2\nendreplace"
    val parser = MyParser()
    val result = parser.parse(parser.command, input)
    val expected = Replace(Tile(0x2e, Dir(Sign.-, Times.Two)))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("replace with conds") {
    val input  = "replace\n  tile 1f-0\n  when 0 0 is 9+2\n  when -1 0 is 3-0\nendreplace"
    val parser = MyParser()
    val result = parser.parse(parser.command, input)
    val expected = Replace(Tile(0x1f, Dir(Sign.-, Times.Zero)), Seq(Pos.zero is TileMatcher(9, Dir(Sign.+, Times.Two)), Pos(-1, 0) is TileMatcher(3, Dir(Sign.-, Times.Zero))))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("replace with conds") {
    val input  = "replace\n  tile 1f-0\n  when\n    0 0 is 9+2\n    -1 0 is 3-0\nendreplace"
    val parser = MyParser()
    val result = parser.parse(parser.command, input)
    val expected = Replace(Tile(0x1f, Dir(Sign.-, Times.Zero)), Seq(Pos.zero is TileMatcher(9, Dir(Sign.+, Times.Two)), Pos(-1, 0) is TileMatcher(3, Dir(Sign.-, Times.Zero))))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
}

class ParseComment extends FunSuite {
  test("comment with #") {
    val input    = "# this is my comment\n"
    val parser   = MyParser()
    val result   = parser.parse(parser.comment, input)
    val expected = Comment(" this is my comment")
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("comment with //") {
    val input    = "// this is my comment\n"
    val parser   = MyParser()
    val result   = parser.parse(parser.comment, input)
    val expected = Comment(" this is my comment")
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("comment containing special chars") {
    val input    = """// this is my comment: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~""" + "\n"
    val parser   = MyParser()
    val result   = parser.parse(parser.comment, input)
    val expected = Comment(" this is my comment: !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
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

class ParseRuleName extends FunSuite {
  test("rule name") {
    val input = "[test]"
    val parser = MyParser()
    val result = parser.parse(parser.ruleName, input)
    val expected = "test"
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
}
