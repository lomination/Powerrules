package lomination.powerrules.lexing

import lomination.powerrules.FunSuite
import lomination.powerrules.ImplicitConversions.given
import lomination.powerrules.config.Config
import lomination.powerrules.lexing.Lexer.Segment
import lomination.powerrules.lexing.tokens._

import scala.language.implicitConversions

class LexerTest extends FunSuite {

  val cfg = Config()

  test("apply method 1 - empty source") {
    val source   = ""
    val obtained = Lexer(source)(using cfg)
    val expected = Seq()
    assert(obtained.isSuccess)
    assert(clue(obtained.get) == clue(expected))
  }

  test("apply method 2 - one line source") {
    val source   = "Hello there!"
    val obtained = Lexer(source)(using cfg)
    val expected = Seq(
      Literal("Hello", (1, 1), (1, 6)),
      Space(" ", (1, 6), (1, 7)),
      Literal("there", (1, 7), (1, 12)),
      Unknown("!", (1, 12), (1, 13))
    )
    assert(obtained.isSuccess)
    assert(clue(obtained.get) == clue(expected))
  }

  test("apply method 3 - multi line source with comment") {
    val source = """|Hello there!
                    |
                    |// some comment""".stripMargin
    val obtained = Lexer(source)(using cfg)
    val expected = Seq(
      Literal("Hello", (1, 1), (1, 6)),
      Space(" ", (1, 6), (1, 7)),
      Literal("there", (1, 7), (1, 12)),
      Unknown("!", (1, 12), (1, 13)),
      Newline("\n", (1, 13), (2, 1)),
      Newline("\n", (2, 1), (3, 1))
    )
    assert(obtained.isSuccess)
    assert(clue(obtained.get) == clue(expected))
  }

  test("apply method 4 - multi line source with comment") {
    val source = """|Hello there!
                    |/*
                    |some comment
                    |*/
                    |Hello there!""".stripMargin
    val obtained = Lexer(source)(using cfg)
    val expected = Seq(
      Literal("Hello", (1, 1), (1, 6)),
      Space(" ", (1, 6), (1, 7)),
      Literal("there", (1, 7), (1, 12)),
      Unknown("!", (1, 12), (1, 13)),
      Newline("\n", (1, 13), (2, 1)),
      Newline("\n", (4, 3), (5, 1)),
      Literal("Hello", (5, 1), (5, 6)),
      Space(" ", (5, 6), (5, 7)),
      Literal("there", (5, 7), (5, 12)),
      Unknown("!", (5, 12), (5, 13))
    )
    assert(obtained.isSuccess)
    assert(clue(obtained.get) == clue(expected))
  }

  test("segment method 1 - empty source") {
    val source   = ""
    val obtained = Lexer.segment(source)
    val expected = Seq()
    assert(obtained.isSuccess)
    assert(clue(obtained.get) == clue(expected))
  }

  test("segment method 2 - single line source") {
    val source   = "This is a segment method! ***"
    val obtained = Lexer.segment(source)
    val expected = Seq(
      Segment("This", (1, 1), (1, 5)),
      Segment(" ", (1, 5), (1, 6)),
      Segment("is", (1, 6), (1, 8)),
      Segment(" ", (1, 8), (1, 9)),
      Segment("a", (1, 9), (1, 10)),
      Segment(" ", (1, 10), (1, 11)),
      Segment("segment", (1, 11), (1, 18)),
      Segment(" ", (1, 18), (1, 19)),
      Segment("method", (1, 19), (1, 25)),
      Segment("!", (1, 25), (1, 26)),
      Segment(" ", (1, 26), (1, 27)),
      Segment("*", (1, 27), (1, 28)),
      Segment("*", (1, 28), (1, 29)),
      Segment("*", (1, 29), (1, 30))
    )
    assert(obtained.isSuccess)
    assert(clue(obtained.get) == clue(expected))
  }

  test("segment method 3 - multi line source") {
    val source = """|This is a segment method! ***
                    |//// @^çà~4?:+)}
                    |""".stripMargin
    val obtained = Lexer.segment(source)
    val expected = Seq(
      Segment("This", (1, 1), (1, 5)),
      Segment(" ", (1, 5), (1, 6)),
      Segment("is", (1, 6), (1, 8)),
      Segment(" ", (1, 8), (1, 9)),
      Segment("a", (1, 9), (1, 10)),
      Segment(" ", (1, 10), (1, 11)),
      Segment("segment", (1, 11), (1, 18)),
      Segment(" ", (1, 18), (1, 19)),
      Segment("method", (1, 19), (1, 25)),
      Segment("!", (1, 25), (1, 26)),
      Segment(" ", (1, 26), (1, 27)),
      Segment("*", (1, 27), (1, 28)),
      Segment("*", (1, 28), (1, 29)),
      Segment("*", (1, 29), (1, 30)),
      Segment("\n", (1, 30), (2, 1)),
      Segment("/", (2, 1), (2, 2)),
      Segment("/", (2, 2), (2, 3)),
      Segment("/", (2, 3), (2, 4)),
      Segment("/", (2, 4), (2, 5)),
      Segment(" ", (2, 5), (2, 6)),
      Segment("@", (2, 6), (2, 7)),
      Segment("^", (2, 7), (2, 8)),
      Segment("ç", (2, 8), (2, 9)),
      Segment("à", (2, 9), (2, 10)),
      Segment("~", (2, 10), (2, 11)),
      Segment("4", (2, 11), (2, 12)),
      Segment("?", (2, 12), (2, 13)),
      Segment(":", (2, 13), (2, 14)),
      Segment("+", (2, 14), (2, 15)),
      Segment(")", (2, 15), (2, 16)),
      Segment("}", (2, 16), (2, 17)),
      Segment("\n", (2, 17), (3, 1))
    )
    assert(obtained.isSuccess)
    assert(clue(obtained.get) == clue(expected))
  }

  test("tokenize method 1 - emtpy source") {
    val source   = Seq()
    val obtained = Lexer.tokenize(source)
    val expected = Seq()
    assert(obtained.isSuccess)
    assert(clue(obtained.get) == clue(expected))
  }

  test("tokenize method 2") {
    val source = Seq(
      Segment("replace", (1, 1), (1, 8)),
      Segment(" ", (1, 8), (1, 9)),
      Segment("with", (1, 9), (1, 13)),
      Segment(" ", (1, 13), (1, 14)),
      Segment("0x32", (1, 14), (1, 18)),
      Segment("+", (1, 18), (1, 19)),
      Segment("2", (1, 19), (1, 20))
    )
    val obtained = Lexer.tokenize(source)
    val expected = Seq(
      Literal("replace", (1, 1), (1, 8)),
      Space(" ", (1, 8), (1, 9)),
      Literal("with", (1, 9), (1, 13)),
      Space(" ", (1, 13), (1, 14)),
      HexaNumber(0x32, "0x32", (1, 14), (1, 18)),
      Plus("+", (1, 18), (1, 19)),
      DecimalNumber(2, "2", (1, 19), (1, 20))
    )
    assert(obtained.isSuccess)
    assert(clue(obtained.get) == clue(expected))
  }

}
