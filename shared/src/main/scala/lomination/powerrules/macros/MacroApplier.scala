package lomination.powerrules.macros

import lomination.powerrules.lexing.TokenParser
import lomination.powerrules.lexing.tokens.{Comma, Dollar, RightParenthese, Token}
import lomination.powerrules.util.split

import scala.util.Try

object MacroApplier extends TokenParser {

  def apply(tokens: Seq[Token], macros: Seq[Macro]): Try[Seq[Token]] =
    if (macros.isEmpty) scala.util.Success(tokens)
    else
      val macrosMap = macros.map(m => (m.name.content, m)).toMap
      parse(macroCalls(macrosMap), TokenReader(tokens)) match
        case Success(tokens, _)     => scala.util.Success(tokens)
        case NoSuccess.I(msg, next) => scala.util.Failure(Exception(msg + s" at ${next.pos}"))

  // ---------- Parser extensions and functions ---------- //

  def optWithBraces[A](parser: P[A]): P[A] =
    leftBraceTk ~> parser <~ rightBraceTk | parser

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
    dollarTk ~! optWithBraces(literalTk ~ withParentheses(macroArgs(macrosMap)).?)
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
