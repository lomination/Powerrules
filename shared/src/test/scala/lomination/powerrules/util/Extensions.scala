package lomination.powerrules.util

import lomination.powerrules.util.*
import lomination.powerrules.FunSuite

class ExtentionsTest extends FunSuite {

  test("ExtentionsTest - split method (1)") {
    val test     = Seq(1, 2, 3, 4, 3, 2, 1).split(2)
    val expected = Seq(Seq(1), Seq(3, 4, 3), Seq(1))
    assert(clue(test) == clue(expected))
  }

  test("ExtentionsTest - split method (2): split of uncontained element") {
    val test     = Seq(1, 2, 3, 4, 3, 2, 1).split(15)
    val expected = Seq(Seq(1, 2, 3, 4, 3, 2, 1))
    assert(clue(test) == clue(expected))
  }

  test("ExtentionsTest - split method (3): split empty seq") {
    val test     = Seq().split(0)
    val expected = Seq(Seq())
    assert(clue(test) == clue(expected))
  }

  test("ExtentionsTest - i method (1)") {
    val test     = "abc".i.regex
    val expected = "[aA][bB][cC]"
    assert(clue(test) == clue(expected))
  }

  test("ExtentionsTest - i method (2): empty regex") {
    val test     = "".i.regex
    val expected = ""
    assert(clue(test) == clue(expected))
  }

  test("ExtentionsTest - i method (3)") {
    val test     = "@a6Ui_".i.regex
    val expected = "@[aA]6[uU][iI]_"
    assert(clue(test) == clue(expected))
  }

}
