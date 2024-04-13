package lomination.ddnettools.parser

import munit.FunSuite
import lomination.ddnettools.*

class ParseReplace extends FunSuite {
  test("replace") {
    val input    = "replace with 2e-2"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.command, input)
    val expected = Replace(Tile(0x2e, Dir.m2))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("replace using re") {
    val input    = "re with 2e-2"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.command, input)
    val expected = Replace(Tile(0x2e, Dir.m2))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("indented replace") {
    val input    = "replace\n  with\n    2e-2"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.command, input)
    val expected = Replace(Tile(0x2e, Dir.m2))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("replace with conds") {
    val input    = "replace\n  with 1f-0\n  if 0 0 is 9+2 & -1 0 is 3-0"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.command, input)
    val expected = Replace(Tile(0x1f, Dir.m0), (Pos.zero is TileMatcher(9, Dir.p2)) & (Pos(-1, 0) is TileMatcher(3, Dir.m0)))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("indented replace with conds") {
    val input    = "replace\n  with 1f-0\n  if\n    0 0 is 9+2 &\n    -1 0 is 3-0"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.command, input)
    val expected = Replace(Tile(0x1f, Dir.m0), (Pos.zero is TileMatcher(9, Dir.p2)) & (Pos(-1, 0) is TileMatcher(3, Dir.m0)))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("complete replace") {
    val input    = "replace\n  with 1f-0\n  when 0 0 is 9+2 & -1 0 is 3-0\n  random 0.33\n  rotate +0+1+2+3"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.command, input)
    val expected = Replace(Tile(0x1f, Dir.m0), (Pos.zero is TileMatcher(9, Dir.p2)) & (Pos(-1, 0) is TileMatcher(3, Dir.m0)), Random(33), Seq(Dir.p0, Dir.p1, Dir.p2, Dir.p3))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
}

class ParseComment extends FunSuite {
  test("comment with #") {
    val input    = "# this is my comment"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.comment, input)
    val expected = Comment(" this is my comment")
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("comment containing special chars") {
    val input    = """# this is my comment: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~"""
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.comment, input)
    val expected = Comment(""" this is my comment: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~""")
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
}

class ParseTile extends FunSuite {
  test("basic tile") {
    val input    = "02+0"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0x02, Dir.p0)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("1 digit tile") {
    val input    = "3+0"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0x3, Dir.p0)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("hexa tile") {
    val input    = "0a+0"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0x0a, Dir.p0)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("1 digit hexa tile") {
    val input    = "b+0"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0xb, Dir.p0)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("positive dir tile") {
    val input    = "a2+2"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0xa2, Dir.p2)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("negative dir tile") {
    val input    = "b3-3"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0xb3, Dir.m3)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
}

class ParseRuleName extends FunSuite {
  test("rule name") {
    val input    = "[test]"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.ruleName, input)
    val expected = "test"
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
}

class ParseDefaultTile extends FunSuite {
  test("basic default tile") {
    val input    = ":ff-3"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.defaultTile, input)
    val expected = DefaultTile(0xff, Dir.m3)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
}
