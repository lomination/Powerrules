package lomination.ddnettools.parser

import munit.FunSuite
import lomination.ddnettools.*

class ParseClear extends FunSuite {
  test("clear") {
    val input    = "clear\n  with 01+0\nendclear"
    val parser   = AutoruleParser()
    val result   = parser.parse(parser.command, input)
    val expected = Clear(Tile(0x01, Dir(Sign.Plus, Times.Zero)))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("indented clear") {
    val input    = "clear\n  with\n    01+0\nendclear"
    val parser   = AutoruleParser()
    val result   = parser.parse(parser.command, input)
    val expected = Clear(Tile(0x01, Dir(Sign.Plus, Times.Zero)))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
}

class ParseReset extends FunSuite {
  test("reset") {
    val input  = "reset\n  with 2e-2\nendreset"
    val parser = AutoruleParser()
    val result = parser.parse(parser.command, input)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(Reset(Tile(0x2e, Dir(Sign.Minus, Times.Two)))))
  }
  test("indented reset") {
    val input  = "reset\n  with\n    2e-2\nendreset"
    val parser = AutoruleParser()
    val result = parser.parse(parser.command, input)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(Reset(Tile(0x2e, Dir(Sign.Minus, Times.Two)))))
  }
}

class ParseReplace extends FunSuite {
  test("replace") {
    val input  = "replace\n  with 1f-0\n  if (0,0) is full\nendreplace"
    val parser = AutoruleParser()
    val result = parser.parse(parser.command, input)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(Replace(Tile(0x1f, Dir(Sign.Minus, Times.Zero)), Seq(Cond(Pos(0, 0), Operator.Equal, TileMatcher(FullTile, AnyDir))))))
  }
  test("indented replace") {
    val input  = "replace\n  with\n    1f-0\n  if\n    (0,0) is full\nendreplace"
    val parser = AutoruleParser()
    val result = parser.parse(parser.command, input)
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(Replace(Tile(0x1f, Dir(Sign.Minus, Times.Zero)), Seq(Cond(Pos(0, 0), Operator.Equal, TileMatcher(FullTile, AnyDir))))))
  }
}

class ParseTile extends FunSuite {
  test("basic") {
    val input    = "02+0"
    val parser   = AutoruleParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0x02, Dir(Sign.Plus, Times.Zero))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("1 digit") {
    val input    = "3+0"
    val parser   = AutoruleParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0x3, Dir(Sign.Plus, Times.Zero))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("hexa") {
    val input    = "0a+0"
    val parser   = AutoruleParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0x0a, Dir(Sign.Plus, Times.Zero))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("1 digit hexa") {
    val input    = "b+0"
    val parser   = AutoruleParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0xb, Dir(Sign.Plus, Times.Zero))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("positive dir") {
    val input    = "a2+2"
    val parser   = AutoruleParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0xa2, Dir(Sign.Plus, Times.Two))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
  test("negative dir") {
    val input    = "b3-3"
    val parser   = AutoruleParser()
    val result   = parser.parse(parser.tile, input)
    val expected = Tile(0xb3, Dir(Sign.Minus, Times.Three))
    assert(result.successful, s"PARSING ERROR: $result")
    assert(clue(result.get) == clue(expected))
  }
}
