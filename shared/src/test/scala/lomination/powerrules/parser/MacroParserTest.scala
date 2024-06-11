package lomination.powerrules.parser

import scala.util.Try
import munit.FunSuite

class UseMacro extends FunSuite {

  val P = MacroParser

  test("UseMacro (1): without params") {
    val input    = """re with $tile"""
    val marcos   = Seq(Macro("tile", Seq(), "23"))
    val result   = P.applyMacro(marcos, input)
    val expected = "re with 23"
    assert(clue(result) == clue(expected))
  }

  test("UseMacro (2): with params") {
    val input    = """$reset(12-2)"""
    val marcos   = Seq(Macro("reset", Seq("tile"), "re with <tile> if 0 0 is full"))
    val result   = P.applyMacro(marcos, input)
    val expected = "re with 12-2 if 0 0 is full"
    assert(clue(result) == clue(expected))
  }

  test("UseMacro (3): wrong number of params") {
    val input  = """re with $tile(a)"""
    val marcos = Seq(Macro("tile", Seq(), "23"))
    val result = Try(P.applyMacro(marcos, input))
    assert(result.isFailure)
    assert(result.failed.get.isInstanceOf[IllegalArgumentException])
  }

  test("UseMacro (4): wrong number of params") {
    val input  = """re with $tile()"""
    val marcos = Seq(Macro("tile", Seq(), "23"))
    val result = Try(P.applyMacro(marcos, input))
    assert(result.isFailure)
    assert(result.failed.get.isInstanceOf[IllegalArgumentException])
  }

  test("UseMacro (5): nested macros") {
    val input = """$reset($tile(-2),$cond(0 0,full))"""
    val marcos = Seq(
      Macro("reset", Seq("tile", "cond"), "re with <tile> if <cond>"),
      Macro("tile", Seq("dir"), "12<dir>"),
      Macro("cond", Seq("pos", "matcher"), "<pos> is <matcher>")
    )
    val result   = P.applyMacro(marcos, input)
    val expected = "re with 12-2 if 0 0 is full"
    assert(clue(result) == clue(expected))
  }

}
