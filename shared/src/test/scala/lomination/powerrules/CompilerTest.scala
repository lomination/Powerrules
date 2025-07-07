package lomination.powerrules

class CompilerTest extends FunSuite {

  test("CompilerTest - getSections method (1): fast mode") {
    val code = """|// wow a comment
                  |
                  |[My Rule]
                  |
                  |bloop bloop bloop
                  |
                  |""".stripMargin
    val test = Compiler.getSections(code)
    assert(clue(test._1) == clue(""))
    assert(clue(test._2) == clue(""))
    assert(clue(test._3) == clue(code))
  }

  test("CompilerTest - getSections method (2): formatted mode") {
    val code = """|
                  |
                  |[My Rule]
                  |
                  |bloop bloop bloop
                  |
                  |""".stripMargin
    val test = Compiler.getSections(code)
    assert(clue(test._1) == clue(""))
    assert(clue(test._2) == clue(""))
    assert(clue(test._3) == clue(code))
  }

  test("CompilerTest - getSections method (3): formatted mode with empty lines") {
    val code =
      """|
         |
         |::rules::
         |
         |[My Rule]
         |
         |bloop bloop bloop
         |
         |""".stripMargin
    val rulesSection =
      """|
         |
         |
         |
         |[My Rule]
         |
         |bloop bloop bloop
         |
         |""".stripMargin
    val test = Compiler.getSections(code)
    assert(clue(test._1) == clue(""))
    assert(clue(test._2) == clue(""))
    assert(clue(test._3) == clue(rulesSection))
  }

  test("CompilerTest - getSections method (4): formatted mode with macro section") {
    val code =
      """|
         |::macro::
         |
         |def mymacro(bloop)
         |    // this is a <bloop>!
         |end
         |
         |::rule::
         |
         |[My Rule]
         |
         |bloop bloop bloop
         |
         |""".stripMargin
    val macrosSection =
      """|
         |
         |
         |def mymacro(bloop)
         |    // this is a <bloop>!
         |end
         |
         |""".stripMargin
    val rulesSection =
      """|
         |
         |
         |
         |
         |
         |
         |
         |
         |[My Rule]
         |
         |bloop bloop bloop
         |
         |""".stripMargin
    val test = Compiler.getSections(code)
    assert(clue(test._1) == clue(""))
    assert(clue(test._2) == clue(macrosSection))
    assert(clue(test._3) == clue(rulesSection))
  }

}
