package lomination.ddnettools.parser

import scala.util.Try
import munit.FunSuite

class UseMacro extends FunSuite {
  // test("apply macro without params") {
  //   val input    = "re with $tile"
  //   val macroSeq = Seq(Macro("tile", Seq(), "23"))
  //   val parser   = MacroParser()
  //   val result   = parser.applyMacro(macroSeq, input)
  //   val expected = "re with 23"
  //   assert(clue(result) == clue(expected))
  // }
  // test("apply macro with wrong num of params") {
  //   val input    = "re with $tile(a)"
  //   val macroSeq = Seq(Macro("tile", Seq(), "23"))
  //   val parser   = MacroParser()
  //   val result   = Try( parser.applyMacro(macroSeq, input) )
  //   assert(result.isFailure)
  //   assert(result.failed.get.isInstanceOf[IllegalArgumentException])
  // }
  // test("apply macro with wrong num of params (1)") {
  //   val input    = "re with $tile()"
  //   val macroSeq = Seq(Macro("tile", Seq(), "23"))
  //   val parser   = MacroParser()
  //   val result   = Try( parser.applyMacro(macroSeq, input) )
  //   assert(result.isFailure)
  //   assert(result.failed.get.isInstanceOf[IllegalArgumentException])
  // }
  // test("apply macro with params") {
  //   val input    = "$reset(12-2)"
  //   val macroSeq = Seq(Macro("reset", Seq("tile"), "re with <tile> if 0 0 is full"))
  //   val parser   = MacroParser()
  //   val result   = parser.applyMacro(macroSeq, input)
  //   val expected = "re with 12-2 if 0 0 is full"
  //   assert(clue(result) == clue(expected))
  // }
  test("apply macro with macros as params") {
    val input    = "$reset($tile(-2),$cond(0 0,full))"
    val macroSeq = Seq(
      Macro("reset", Seq("tile", "cond"), "re with <tile> if <cond>"),
      Macro("tile", Seq("dir"), "12<dir>"),
      Macro("cond", Seq("pos", "matcher"), "<pos> is <matcher>")
    )
    val parser   = MacroParser()
    val result   = parser.applyMacro(macroSeq, input)
    val expected = "re with 12-2 if 0 0 is full"
    assert(clue(result) == clue(expected))
  }
}
