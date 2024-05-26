package lomination.powerrules.parser

import munit.FunSuite

class TestPreProcessing extends FunSuite {
  test("pre process comments") {
    val input    = "bloop\n// bibi\nbaba"
    val parser   = MyParser()
    val result   = parser.preProcess(input)
    val expected = "bloop\n\nbaba"
    assert(clue(result) == clue(expected))
  }
  test("pre process comments (1)") {
    val input    = "bloop\n/* bibi\nbaba */bubu"
    val parser   = MyParser()
    val result   = parser.preProcess(input)
    val expected = "bloop\nbubu"
    assert(clue(result) == clue(expected))
  }
}
