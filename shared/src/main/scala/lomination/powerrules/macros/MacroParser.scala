package lomination.powerrules.macros

import lomination.powerrules.util.dropOnce
import lomination.powerrules.lexing.TokenParser
import scala.util.Try
import lomination.powerrules.lexing.tokens.{End, Token}
import scala.annotation.tailrec

object MacroParser extends TokenParser {

  def apply(tokens: Seq[Token]): Try[Seq[Macro]] =
    if (tokens.isEmpty)
      scala.util.Success(Seq())
    else
      for {
        macros        <- parseMacros(tokens)
        appliedMacros <- applyMacros(macros)
      } yield appliedMacros

  def parseMacros(tokens: Seq[Token]): Try[Seq[Macro]] =
    parse(macroSection, TokenReader(tokens)) match
      case Success(result, next) =>
        scala.util.Success(result)
      case NoSuccess.I(msg, next) =>
        val exc = MacroError(s"error near ${next.pos}:\n$msg")
        scala.util.Failure(exc)

  /** Computes the macro calls inside of the contents of the parsed macros */
  def applyMacros(macros: Seq[Macro]): Try[Seq[Macro]] =
    @tailrec
    def process(cooked: Seq[Macro], raw: Seq[Macro]): Try[Seq[Macro]] =
      if (raw.isEmpty) scala.util.Success(cooked)
      else
        val m = raw.head
        MacroApplier(m.content, cooked) match
          case scala.util.Success(tokens) =>
            process(cooked :+ Macro(m.name, m.paramNames, tokens), raw.dropOnce)
          case scala.util.Failure(exception) =>
            scala.util.Failure(exception)
    process(Seq(), macros)

  // ---------- Parsers ---------- //

  def block[A](parser: P[A]): P[A] =
    indentTk ~> parser <~ dedentTk

  def withParentheses[A](parser: P[A]): P[A] =
    leftParentheseTk ~>! parser <~ rightParentheseTk

  def optSpaced[A](parser: P[A]): P[A] =
    spaceTk.? ~> parser <~ spaceTk.?

  def macroSection: P[Seq[Macro]] =
    phrase(newlineTk.? ~> repsep(macroDefinition, newlineTk) <~ newlineTk.?)

  def macroDefinition: P[Macro] =
    defTk ~ spaceTk
      ~>! literalTk
      ~! withParentheses(spaceTk.? ~> repsep(literalTk, optSpaced(commaTk)) <~ spaceTk.?).?
      ~! (spaceTk.? ~ guard(indentTk) ~>! rep(notEndTk))
      <~ endTk
      >> { case name ~ params ~ content =>
        Macro.safeBuild(name, params.getOrElse(Seq()), content) match
          case scala.util.Success(value)     => success(value)
          case scala.util.Failure(exception) => err(exception.getMessage)
      } |< "macro definition"

  def notEndTk: P[Token] =
    acceptMatch("any token except `end` keyword", { case token if !token.isInstanceOf[End] => token })
      |< "any token except `end` keyword"

}
