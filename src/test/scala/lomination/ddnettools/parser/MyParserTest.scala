package lomination.ddnettools.parser

import munit.FunSuite
import lomination.ddnettools.*

class ParseReplace extends FunSuite {
  test("basic replace") {
    val input    = "replace\n  tile 2e-2\nendreplace"
    val parser   = MyParser()
    val result   = parser.parse(parser.command, input)
    val expected = Replace(Tile(0x2e, Dir.m2))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("indented replace") {
    val input    = "replace\n  tile\n    2e-2\nendreplace"
    val parser   = MyParser()
    val result   = parser.parse(parser.command, input)
    val expected = Replace(Tile(0x2e, Dir.m2))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("replace with conds") {
    val input    = "replace\n  tile 1f-0\n  when 0 0 is 9+2\n  when -1 0 is 3-0\nendreplace"
    val parser   = MyParser()
    val result   = parser.parse(parser.command, input)
    val expected = Replace(Tile(0x1f, Dir.m0), Seq(Pos.zero is TileMatcher(9, Dir.p2), Pos(-1, 0) is TileMatcher(3, Dir.m0)))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("indented replace with conds") {
    val input    = "replace\n  tile 1f-0\n  when\n    0 0 is 9+2\n    -1 0 is 3-0\nendreplace"
    val parser   = MyParser()
    val result   = parser.parse(parser.command, input)
    val expected = Replace(Tile(0x1f, Dir.m0), Seq(Pos.zero is TileMatcher(9, Dir.p2), Pos(-1, 0) is TileMatcher(3, Dir.m0)))
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
    val expected = Comment(""" this is my comment: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~""")
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
}

class ParseTile extends FunSuite {
  test("basic tile") {
    val input    = "02+0"
    val parser   = MyParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0x02, Dir.p0)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("1 digit tile") {
    val input    = "3+0"
    val parser   = MyParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0x3, Dir.p0)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("hexa tile") {
    val input    = "0a+0"
    val parser   = MyParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0x0a, Dir.p0)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("1 digit hexa tile") {
    val input    = "b+0"
    val parser   = MyParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0xb, Dir.p0)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("positive dir tile") {
    val input    = "a2+2"
    val parser   = MyParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0xa2, Dir.p2)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("negative dir tile") {
    val input    = "b3-3"
    val parser   = MyParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0xb3, Dir.m3)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
}

class ParseRuleName extends FunSuite {
  test("rule name") {
    val input    = "[test]"
    val parser   = MyParser()
    val result   = parser.parse(parser.ruleName, input)
    val expected = "test"
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
}

class ParseDefaultTile extends FunSuite {
  test("basic default tile") {
    val input    = "$defaultTile = ff-3\n"
    val parser   = MyParser()
    val result   = parser.parse(parser.defaultTile, input)
    val expected = DefaultTile(255, Dir.m3)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("default tile with whitespace") {
    val input    = "$defaultTile   =   ff-0\n"
    val parser   = MyParser()
    val result   = parser.parse(parser.defaultTile, input)
    val expected = DefaultTile(255, Dir.m0)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
}
