package lomination.powerrules.macros

import lomination.powerrules.util.split
import lomination.powerrules.util.style.{ansi0, ansi4}
import lomination.powerrules.lexing.tokens.{Comma, Dollar, RightParenthese, Token}
import scala.util.Try
import scala.util.parsing.input.Reader
import lomination.powerrules.lexing.TokenParser
import scala.annotation.tailrec
import scala.collection.mutable.Builder

object MacroApplier extends TokenParser {

  def apply(tokens: Seq[Token], macros: Seq[Macro]): Try[Seq[Token]] =
    if (macros.isEmpty) scala.util.Success(tokens)
    else
      val macrosMap = macros.map(m => (m.name.content, m)).toMap
      parse(macroCalls(macrosMap), TokenReader(tokens)) match
        case Success(tokens, _)     => scala.util.Success(tokens)
        case NoSuccess.I(msg, next) => scala.util.Failure(Exception(msg + s" at ${next.pos}"))

  // ---------- Parser extensions and functions ---------- //

  def optWithAcolades[A](parser: P[A]): P[A] =
    leftAcoladeTk ~> parser <~ rightAcoladeTk | parser

  def withParentheses[A](parser: P[A]): P[A] =
    leftParentheseTk ~> parser <~ rightParentheseTk

  def notDollar: P[Token] =
    acceptMatch("any token except dollar sign `$`", { case token if !token.isInstanceOf[Dollar] => token })
      |< "any token except dollar sign `$`"

  def notDollarRp: P[Token] =
    acceptMatch("any token except dollar sign `$`", { case token if !token.isInstanceOf[Dollar] && !token.isInstanceOf[RightParenthese] => token })
      |< "any token except dollar sign `$`"

  def macroCalls(macrosMap: Map[String, Macro]): P[Seq[Token]] =
    rep(notDollar ^^ { Seq(_) } | macroCall(macrosMap)) ^^ { _.flatMap(identity) }

  def macroCall(macrosMap: Map[String, Macro]): P[Seq[Token]] =
    dollarTk ~! optWithAcolades(literalTk ~ withParentheses(macroArgs(macrosMap)).?)
      >> { case Dollar(_, callStartPos, callEndPos) ~ (name ~ args) =>
        macrosMap.get(name.content) match
          case Some(m) =>
            m.apply(args.map(_.split(_.isInstanceOf[Comma])).getOrElse(Seq()), callStartPos, callEndPos) match
              case scala.util.Success(tokens) => success(tokens)
              case _                          => err("Macro invokation has failed")
          case _ => err(s"Definition of macro ${name.content} has not been found")
      }
      |< "macro call"

  def macroArgs(macrosMap: Map[String, Macro]): P[Seq[Token]] =
    rep(notDollarRp ^^ { Seq(_) } | macroCall(macrosMap)) ^^ { _.flatMap(identity) }
      |< "macro arguments"
}
