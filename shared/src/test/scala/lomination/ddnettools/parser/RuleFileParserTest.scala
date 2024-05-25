package lomination.ddnettools.parser

import munit.FunSuite
import lomination.ddnettools.*

class ParseReplace extends FunSuite {
  test("replace") {
    val input    = "replace with 2e-2"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.command, input)
    val expected = Replace(Seq(Tile(0x2e, Dir.m2)))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("replace using re") {
    val input    = "re with 2e-2"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.command, input)
    val expected = Replace(Seq(Tile(0x2e, Dir.m2)))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("indented replace") {
    val input    = "replace\n  with\n    2e-2"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.command, input)
    val expected = Replace(Seq(Tile(0x2e, Dir.m2)))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("replace with conds") {
    val input    = "replace\n  with 1f-0\n  if 0 0 is 9+2 & -1 0 is 3-0"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.command, input)
    val expected = Replace(Seq(Tile(0x1f, Dir.m0)), (Pos.zero is TileMatcher(9, Dir.p2)) & (Pos(-1, 0) is TileMatcher(3, Dir.m0)))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("indented replace with conds") {
    val input    = "replace\n  with 1f-0\n  if\n    0 0 is 9+2 &\n    -1 0 is 3-0"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.command, input)
    val expected = Replace(Seq(Tile(0x1f, Dir.m0)), (Pos.zero is TileMatcher(9, Dir.p2)) & (Pos(-1, 0) is TileMatcher(3, Dir.m0)))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("complete replace") {
    val input    = "replace\n  with 1f-0\n  when 0 0 is 9+2 & -1 0 is 3-0\n  random 0.33\n  rotate +0+1+2+3"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.command, input)
    val expected = Replace(Seq(Tile(0x1f, Dir.m0)), (Pos.zero is TileMatcher(9, Dir.p2)) & (Pos(-1, 0) is TileMatcher(3, Dir.m0)), Random(33), Seq(Dir.p0, Dir.p1, Dir.p2, Dir.p3))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
}

class ParseShadow extends FunSuite {
  test("shadow") {
    val input  = "shadow with 1 14+1 75+1 24+1 76+2 52 if 0 0 is full"
    val parser = RuleFileParser()
    val result = parser.parse(parser.shadow, input)
    val expected = Shadow(
      Seq(Tile(0x1), Tile(0x14, Dir.p1), Tile(0x75, Dir.p1), Tile(0x24, Dir.p1), Tile(0x76, Dir.p2), Tile(0x52)),
      Seq(Pos(0, 0) is FullMatcher(Op.Is)),
      ShadowType.default
    )
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("shadow -e+i") {
    val input = "shadow\n" +
      "  with 1 14+1 75+1 24+1 76+2 52 35+2 85+2 b0+1 b1+3 b2 b3+1 60+1 b4\n" +
      "  if 0 0 is full & 0 -1 is empty\n" +
      "  type -e+i"
    val parser = RuleFileParser()
    val result = parser.parse(parser.shadow, input)
    val expected = Shadow(
      Seq(
        Tile(0x1),
        Tile(0x14, Dir.p1),
        Tile(0x75, Dir.p1),
        Tile(0x24, Dir.p1),
        Tile(0x76, Dir.p2),
        Tile(0x52),
        Tile(0x35, Dir.p2),
        Tile(0x85, Dir.p2),
        Tile(0xb0, Dir.p1),
        Tile(0xb1, Dir.p3),
        Tile(0xb2),
        Tile(0xb3, Dir.p1),
        Tile(0x60, Dir.p1),
        Tile(0xb4)
      ),
      (Pos(0, 0) is FullMatcher(Op.Is)) & (Pos(0, -1) is FullMatcher(Op.Isnot)),
      ShadowType(false, true)
    )
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
}

class ParseShape extends FunSuite {
  test("shape") {
    val input = "shape\n" +
      "  apply\n" +
      "    1 2\n" +
      "    3 4\n" +
      "  on\n" +
      "    . .\n" +
      "    . .\n" +
      "  using\n" +
      "    1 -> 1+0\n" +
      "    2 -> 2+0\n" +
      "    3 -> 3+0\n" +
      "    4 -> 4+0\n" +
      "  neutral 5+0\n" +
      "  random 50%"
    val parser = RuleFileParser()
    val result = parser.parse(parser.shape, input)
    val expected = Shape(
      Grid(Seq(Seq(Some(Tile(1)), Some(Tile(2))), Seq(Some(Tile(3)), Some(Tile(4))))),
      Grid(Seq(Seq(Some(FullMatcher(Op.Is)), Some(FullMatcher(Op.Is))), Seq(Some(FullMatcher(Op.Is)), Some(FullMatcher(Op.Is))))),
      Tile(5),
      Random(50f)
    )
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get.applyPat) == clue(expected.applyPat))
    assert(clue(result.get.onPat) == clue(expected.onPat))
    assert(clue(result.get.neutral) == clue(expected.neutral))
  }
}

class ParseComment extends FunSuite {
  test("comment with #") {
    val input    = "# this is my comment"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.comment, input)
    val expected = Comment("# this is my comment")
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("comment containing special chars") {
    val input    = """# this is my comment: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~"""
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.comment, input)
    val expected = Comment("""# this is my comment: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~""")
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
  test("rule name with brackets") {
    val input    = "[test with [brackets]]"
    val parser   = RuleFileParser()
    val result   = parser.parse(parser.ruleName, input)
    val expected = "test with [brackets]"
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
