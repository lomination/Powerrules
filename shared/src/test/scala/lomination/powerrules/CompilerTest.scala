package lomination.powerrules

import lomination.powerrules.build.BuildInfo

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

  test("CompilerTest - apply method (1)") {
    val code =
      """|
         |::macro::
         |
         |def macro(comment)
         |    # this is a comment: <comment>
         |end
         |
         |::rule::
         |
         |[My Rule]
         |
         |$macro(hello)
         |
         |""".stripMargin
    val test = Compiler(code)
    val expected =
      s"""|# Generated with PowerRules (version ${BuildInfo.version}) by lomination
          |# https://github.com/lomination/powerrules
          |
          |
          |
          |[My Rule]
          |
          |# this is a comment: hello
          |""".stripMargin
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("CompilerTest - apply method (2)") {
    val code =
      """|[My Rule]
         |
         |replace
         |    with 1
         |    if there is 1 or 2 or 3
         |
         |""".stripMargin
    val test = Compiler(code)
    val expected =
      s"""|# Generated with PowerRules (version ${BuildInfo.version}) by lomination
          |# https://github.com/lomination/powerrules
          |
          |
          |
          |[My Rule]
          |
          |Index 1 NONE
          |Pos 0 0 INDEX 1 OR 2 OR 3
          |NewRun
          |""".stripMargin
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

}
