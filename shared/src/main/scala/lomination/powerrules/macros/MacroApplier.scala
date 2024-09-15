package lomination.powerrules.macros

import lomination.powerrules.util.split
import lomination.powerrules.util.style.{ansi4, ansi0}
import lomination.powerrules.lexing.tokens.{Token, Dollar, RightParenthese, Comma}
import scala.util.Try
import scala.util.parsing.input.Reader
import lomination.powerrules.lexing.TokenParser
import scala.annotation.tailrec

object MacroApplier extends TokenParser {

  def apply(tokens: Seq[Token], macros: Seq[Macro]): Try[Seq[Token]] =
    if (macros.isEmpty)
      scala.util.Success(tokens)
    else
      val macrosMap =
        macros.map(m => (m.name.content, m)).toMap
      process(Seq(), TokenReader(tokens), macrosMap)

  @tailrec def process(cooked: Seq[Token], raw: Reader[Token], macros: Map[String, Macro]): Try[Seq[Token]] =
    if (raw.atEnd)
      scala.util.Success(cooked)
    else
      raw.first match
        case Dollar(_, start, _) =>
          parse(macroCall, raw) match
            case Success((name, parameters), next) if macros.contains(name) =>
              macros(name)(parameters, start) match
                case scala.util.Success(result)    =>
                  logger debug s"Macro $name successfully applied at $ansi4$start$ansi0"
                  process(cooked ++ result, next, macros)
                case scala.util.Failure(exception) =>
                  logger.error(exception)(s"Macro $name failed to be applied at $ansi4$start$ansi0")
                  scala.util.Failure(exception)
            case Success((name, parameters), next) =>
              val e = MacroError(s"Macro $name is not defined (yet?) at $start")
              logger.error(e)(s"Macro `$name` not found at $ansi4$start$ansi0")
              scala.util.Failure(e)
            case NoSuccess.I(msg, next) =>
              val e = MacroError(s"Failed to parse macro call at $start:\n$msg")
              logger.error(e)(s"Failed to parse macro call at $ansi4$start$ansi0")
              scala.util.Failure(e)
        case token =>
          process(cooked :+ token, raw.rest, macros)

  // ---------- Parser extensions and functions ---------- //

  def optWithAcolades[A](parser: P[A]): P[A] =
    leftAcoladeTk ~> parser <~ rightAcoladeTk | parser

  def withParentheses[A](parser: P[A]): P[A] =
    leftParentheseTk ~> parser <~ rightParentheseTk

  def macroCall: P[(String, Seq[Seq[Token]])] =
    dollarTk ~> optWithAcolades(literalTk ~ withParentheses(notRightParenthese.*).?)
      ^^ { case name ~ params =>
        (name.content, params.getOrElse(Seq()).split(_.isInstanceOf[Comma]))
      }
      |< "macro call"

  def notRightParenthese: P[Token] =
    acceptMatch("any token except right parenthese character `)`", { case token if !token.isInstanceOf[RightParenthese] => token })
      |< "any token except right parenthese character `)`"

}
