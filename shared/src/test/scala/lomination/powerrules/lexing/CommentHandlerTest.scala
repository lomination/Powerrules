package lomination.powerrules.lexing

import lomination.powerrules.FunSuite
import lomination.powerrules.Functions.{build, given_Conversion_Int_TestPos}
import lomination.powerrules.lexing.tokens._

import scala.language.implicitConversions

class CommentHandlerTest extends FunSuite {

  test("apply method 1 - empty input") {
    val test     = CommentHandler(Seq())
    val expected = Seq()
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("apply method 2 - no comment") {
    val tokens = build(
      Literal("Hello", _, _),
      Comma(",", _, _),
      Space(" ", _, _),
      HexaNumber(0x4f, "0x4f", _, _),
      Plus("+", _, _),
      DecimalNumber(3, "3", _, _),
      Unknown("=", _, _),
      HexaNumber(0x52, "0x52", _, _),
      Unknown("!", _, _)
    )
    val test     = CommentHandler(tokens)
    val expected = tokens
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("apply method 3 - no comment") {
    val tokens = build(
      Literal("Hello", _, _),
      Comma(",", _, _),
      Newline("\n", _, _),
      HexaNumber(0x4f, "0x4f", _, _),
      Plus("+", _, _),
      DecimalNumber(3, "3", _, _),
      Unknown("=", _, _),
      HexaNumber(0x52, "0x52", _, _),
      Unknown("!", _, _),
      Newline("\n", _, _),
      Newline("\n", _, _),
      Dollar("$", _, _),
      Newline("\n", _, _)
    )
    val test     = CommentHandler(tokens)
    val expected = tokens
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("apply method 4 - single line comment") {
    val tokens = build(
      Literal("Hello", _, _),
      Comma(",", _, _),
      Newline("\n", _, _),
      HexaNumber(0x4f, "0x4f", _, _),
      Plus("+", _, _),
      DecimalNumber(3, "3", _, _),
      Unknown("=", _, _),
      HexaNumber(0x52, "0x52", _, _),
      Unknown("!", _, _),
      Newline("\n", _, _),
      Slash("/", _, _),
      Slash("/", _, _),
      Newline("\n", _, _),
      Dollar("$", _, _),
      Newline("\n", _, _)
    )
    val test = CommentHandler(tokens)
    val expected = Seq(
      Literal("Hello", 0, 1),
      Comma(",", 1, 2),
      Newline("\n", 2, 3),
      HexaNumber(0x4f, "0x4f", 3, 4),
      Plus("+", 4, 5),
      DecimalNumber(3, "3", 5, 6),
      Unknown("=", 6, 7),
      HexaNumber(0x52, "0x52", 7, 8),
      Unknown("!", 8, 9),
      Newline("\n", 9, 10),
      Newline("\n", 12, 13),
      Dollar("$", 13, 14),
      Newline("\n", 14, 15)
    )
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("apply method 5 - single line comment") {
    val tokens = build(
      Literal("Hello", _, _),
      Comma(",", _, _),
      Newline("\n", _, _),
      HexaNumber(0x4f, "0x4f", _, _),
      Plus("+", _, _),
      DecimalNumber(3, "3", _, _),
      Unknown("=", _, _),
      HexaNumber(0x52, "0x52", _, _),
      Unknown("!", _, _),
      Newline("\n", _, _),
      Slash("/", _, _),
      Slash("/", _, _),
      Space(" ", _, _),
      Literal("Hopefully", _, _),
      Unknown("^", _, _),
      Unknown("^", _, _),
      Newline("\n", _, _),
      Dollar("$", _, _),
      Newline("\n", _, _)
    )
    val test = CommentHandler(tokens)
    val expected = Seq(
      Literal("Hello", 0, 1),
      Comma(",", 1, 2),
      Newline("\n", 2, 3),
      HexaNumber(0x4f, "0x4f", 3, 4),
      Plus("+", 4, 5),
      DecimalNumber(3, "3", 5, 6),
      Unknown("=", 6, 7),
      HexaNumber(0x52, "0x52", 7, 8),
      Unknown("!", 8, 9),
      Newline("\n", 9, 10),
      Newline("\n", 16, 17),
      Dollar("$", 17, 18),
      Newline("\n", 18, 19)
    )
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("apply method 6 - single line comment") {
    val tokens = build(
      Literal("Hello", _, _),
      Comma(",", _, _),
      Newline("\n", _, _),
      HexaNumber(0x4f, "0x4f", _, _),
      Plus("+", _, _),
      DecimalNumber(3, "3", _, _),
      Unknown("=", _, _),
      HexaNumber(0x52, "0x52", _, _),
      Unknown("!", _, _),
      Newline("\n", _, _),
      Slash("/", _, _),
      Slash("/", _, _),
      Space(" ", _, _),
      Literal("Hopefully", _, _),
      Space(" ", _, _),
      Unknown("^", _, _),
      Unknown("^", _, _),
      Newline("\n", _, _),
      Slash("/", _, _),
      Slash("/", _, _),
      Space(" ", _, _),
      Literal("Keep", _, _),
      Space(" ", _, _),
      Literal("hope", _, _),
      Newline("\n", _, _),
      Dollar("$", _, _),
      Newline("\n", _, _)
    )
    val test = CommentHandler(tokens)
    val expected = Seq(
      Literal("Hello", 0, 1),
      Comma(",", 1, 2),
      Newline("\n", 2, 3),
      HexaNumber(0x4f, "0x4f", 3, 4),
      Plus("+", 4, 5),
      DecimalNumber(3, "3", 5, 6),
      Unknown("=", 6, 7),
      HexaNumber(0x52, "0x52", 7, 8),
      Unknown("!", 8, 9),
      Newline("\n", 9, 10),
      Newline("\n", 17, 18),
      Newline("\n", 24, 25),
      Dollar("$", 25, 26),
      Newline("\n", 26, 27)
    )
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("apply method 7 - multi line comment") {
    val tokens = build(
      Literal("Hello", _, _),
      Comma(",", _, _),
      Newline("\n", _, _),
      HexaNumber(0x4f, "0x4f", _, _),
      Plus("+", _, _),
      DecimalNumber(3, "3", _, _),
      Unknown("=", _, _),
      HexaNumber(0x52, "0x52", _, _),
      Unknown("!", _, _),
      Newline("\n", _, _),
      Slash("/", _, _),
      Star("*", _, _),
      Space(" ", _, _),
      Literal("Hopefully", _, _),
      Space(" ", _, _),
      Unknown("^", _, _),
      Unknown("^", _, _),
      Newline("\n", _, _),
      Literal("Keep", _, _),
      Space(" ", _, _),
      Literal("hope", _, _),
      Space(" ", _, _),
      Star("*", _, _),
      Slash("/", _, _),
      Newline("\n", _, _),
      Dollar("$", _, _),
      Newline("\n", _, _)
    )
    val test = CommentHandler(tokens)
    val expected = Seq(
      Literal("Hello", 0, 1),
      Comma(",", 1, 2),
      Newline("\n", 2, 3),
      HexaNumber(0x4f, "0x4f", 3, 4),
      Plus("+", 4, 5),
      DecimalNumber(3, "3", 5, 6),
      Unknown("=", 6, 7),
      HexaNumber(0x52, "0x52", 7, 8),
      Unknown("!", 8, 9),
      Newline("\n", 9, 10),
      Newline("\n", 24, 25),
      Dollar("$", 25, 26),
      Newline("\n", 26, 27)
    )
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("apply method 8 - several multi line comments") {
    val tokens = build(
      Literal("Hello", _, _),
      Comma(",", _, _),
      Newline("\n", _, _),
      HexaNumber(0x4f, "0x4f", _, _),
      Plus("+", _, _),
      DecimalNumber(3, "3", _, _),
      Unknown("=", _, _),
      HexaNumber(0x52, "0x52", _, _),
      Unknown("!", _, _),
      Newline("\n", _, _),
      Slash("/", _, _),
      Star("*", _, _),
      Space(" ", _, _),
      Literal("Hopefully", _, _),
      Space(" ", _, _),
      Unknown("^", _, _),
      Unknown("^", _, _),
      Star("*", _, _),
      Slash("/", _, _),
      Newline("\n", _, _),
      Slash("/", _, _),
      Star("*", _, _),
      Space(" ", _, _),
      Literal("Keep", _, _),
      Space(" ", _, _),
      Literal("hope", _, _),
      Star("*", _, _),
      Slash("/", _, _),
      Newline("\n", _, _),
      Dollar("$", _, _),
      Newline("\n", _, _)
    )
    val test = CommentHandler(tokens)
    val expected = Seq(
      Literal("Hello", 0, 1),
      Comma(",", 1, 2),
      Newline("\n", 2, 3),
      HexaNumber(0x4f, "0x4f", 3, 4),
      Plus("+", 4, 5),
      DecimalNumber(3, "3", 5, 6),
      Unknown("=", 6, 7),
      HexaNumber(0x52, "0x52", 7, 8),
      Unknown("!", 8, 9),
      Newline("\n", 9, 10),
      Newline("\n", 19, 20),
      Newline("\n", 28, 29),
      Dollar("$", 29, 30),
      Newline("\n", 30, 31)
    )
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("apply method 9 - nested comments") {
    val tokens = build(
      Literal("Hello", _, _),
      Comma(",", _, _),
      Newline("\n", _, _),
      HexaNumber(0x4f, "0x4f", _, _),
      Plus("+", _, _),
      DecimalNumber(3, "3", _, _),
      Unknown("=", _, _),
      HexaNumber(0x52, "0x52", _, _),
      Unknown("!", _, _),
      Newline("\n", _, _),
      Slash("/", _, _),
      Star("*", _, _),
      Space(" ", _, _),
      Literal("Hopefully", _, _),
      Space(" ", _, _),
      Unknown("^", _, _),
      Unknown("^", _, _),
      Newline("\n", _, _),
      Slash("/", _, _),
      Slash("/", _, _),
      Space(" ", _, _),
      Literal("Keep", _, _),
      Space(" ", _, _),
      Literal("hope", _, _),
      Star("*", _, _),
      Slash("/", _, _),
      Newline("\n", _, _),
      Dollar("$", _, _),
      Newline("\n", _, _)
    )
    val test = CommentHandler(tokens)
    val expected = Seq(
      Literal("Hello", 0, 1),
      Comma(",", 1, 2),
      Newline("\n", 2, 3),
      HexaNumber(0x4f, "0x4f", 3, 4),
      Plus("+", 4, 5),
      DecimalNumber(3, "3", 5, 6),
      Unknown("=", 6, 7),
      HexaNumber(0x52, "0x52", 7, 8),
      Unknown("!", 8, 9),
      Newline("\n", 9, 10),
      Newline("\n", 26, 27),
      Dollar("$", 27, 28),
      Newline("\n", 28, 29)
    )
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("apply method 10 - empty multi line comment") {
    val tokens = build(
      Literal("Hey", _, _),
      Space(" ", _, _),
      Slash("/", _, _),
      Star("*", _, _),
      Star("*", _, _),
      Slash("/", _, _),
      Space(" ", _, _),
      Literal("there", _, _)
    )
    val test = CommentHandler(tokens)
    val expected = Seq(
      Literal("Hey", 0, 1),
      Space(" ", 1, 2),
      Space(" ", 6, 7),
      Literal("there", 7, 8)
    )
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("apply method 11 - multi line comment") {
    val tokens = build(
      Literal("Hey", _, _),
      Space(" ", _, _),
      Slash("/", _, _),
      Star("*", _, _),
      Space(" ", _, _),
      Literal("lomi", _, _),
      Space(" ", _, _),
      Star("*", _, _),
      Slash("/", _, _),
      Space(" ", _, _),
      Literal("there", _, _)
    )
    val test = CommentHandler(tokens)
    val expected = Seq(
      Literal("Hey", 0, 1),
      Space(" ", 1, 2),
      Space(" ", 9, 10),
      Literal("there", 10, 11)
    )
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

  test("apply method 12 - failure") {
    val tokens = build(
      Literal("Hey", _, _),
      Space(" ", _, _),
      Slash("/", _, _),
      Star("*", _, _),
      Space(" ", _, _),
      Literal("there", _, _)
    )
    val test = CommentHandler(tokens)
    assert(test.isFailure)
  }

  test("apply method 13 - failure") {
    val tokens = build(
      Literal("Hey", _, _),
      Space(" ", _, _),
      Slash("/", _, _),
      Star("*", _, _),
      Space(" ", _, _),
      Literal("there", _, _),
      Space(" ", _, _),
      Star("*", _, _)
    )
    val test = CommentHandler(tokens)
    assert(test.isFailure)
  }

  test("apply method 14 - single line comment at the end") {
    val tokens = build(
      Literal("Hey", _, _),
      Newline("\n", _, _),
      Slash("/", _, _),
      Slash("/", _, _),
      Space(" ", _, _),
      Literal("hello", _, _)
    )
    val test = CommentHandler(tokens)
    val expected = Seq(
      Literal("Hey", 0, 1),
      Newline("\n", 1, 2)
    )
    assert(test.isSuccess)
    assert(clue(test.get) == clue(expected))
  }

}
