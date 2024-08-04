package lomination.powerrules.parsing

import lomination.powerrules.FunSuite

class PreProcessing extends FunSuite {

  test("PreProcessing (1): comment") {
    val input = """|bloop
                   |// bibi
                   |baba""".stripMargin
    val result = lomination.powerrules.parsing.GlobalParser.preProcess(input)
    val expected = """|bloop
                      |
                      |baba""".stripMargin
    assert(clue(result) == clue(expected))
  }

  test("PreProcessing (2): multiline comment") {
    val input = """|bloop
                   |/* bibi
                   |baba */bubu""".stripMargin
    val result = lomination.powerrules.parsing.GlobalParser.preProcess(input)
    val expected = """|bloop
                      |bubu""".stripMargin
    assert(clue(result) == clue(expected))
  }

  test("PreProcessing (3): nested comment") {
    val input = """|bloop
                   |/* bibi // bloop
                   |baba */bubu""".stripMargin
    val result = lomination.powerrules.parsing.GlobalParser.preProcess(input)
    val expected = """|bloop
                      |bubu""".stripMargin
    assert(clue(result) == clue(expected))
  }

}
