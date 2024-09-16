package lomination.powerrules.macros

import lomination.powerrules.util.{dropOnce, dropRightOnce}
import lomination.powerrules.util.style.{ansi0, ansi2, ansi4}
import scala.util.{Failure, Success, Try}
import lomination.powerrules.lexing.tokens.Token
import lomination.powerrules.lexing.tokens.*
import lomination.powerrules.macros.ParameterError
import scala.util.parsing.input.{NoPosition, Position}

case class Macro(name: Literal, paramNames: Seq[String], content: Seq[Token]):

  val logger = org.log4s.getLogger

  def apply(paramValues: Seq[Seq[Token]], callPos: Position = NoPosition): Try[Seq[Token]] =
    val vLength = paramValues.size
    val nLength = paramNames.size
    if (vLength != nLength)
      val msg =
        s"Invalid given number of parameters for macro `${name.content}` (defined at `${name.start}`) at $ansi4$callPos$ansi0 (given: $vLength, expected: $nLength)"
      val exception = ParameterError(msg)
      logger.error(exception)(msg)
      Failure(exception)
    else
      val parameters = (paramNames zip paramValues).toMap
      replaceParameters(Seq(), content, parameters)

  def replaceParameters(computedOnes: Seq[Token], nextOnes: Seq[Token], parameters: Map[String, Seq[Token]]): Try[Seq[Token]] =
    nextOnes match
      case LeftChevron(_, _, _) :: Literal(param, _, pos, _) :: RightChevron(_, _, _) :: next =>
        parameters.get(param) match
          case Some(tokens) =>
            logger trace s"parameter $param successfully replaced at $ansi4$pos$ansi0 $ansi2(by `${tokens.map(_.raw).mkString}`)$ansi0 "
            replaceParameters(computedOnes ++ tokens, next, parameters)
          case None =>
            val e = MacroError(s"parameter $param not defined at $pos")
            logger.error(e)(s"Parameter $param at $ansi4$pos$ansi0 is not defined")
            Failure(e)
      case token :: next =>
        logger trace s"Neutral token ${token.getName} found and skipped at $ansi4${token.start}$ansi0"
        replaceParameters(computedOnes :+ token, next, parameters)
      case Nil =>
        Success(computedOnes)

object Macro:

  def safeBuild(name: Literal, paramNames: Seq[Literal], content: Seq[Token]): Try[Macro] =
    if (!content.head.isInstanceOf[Indent]) Failure(MacroError(s"Indentation Error at ${content.head.start}. Should start with an indent token"))
    else if (!content.last.isInstanceOf[Dedent]) Failure(MacroError(s"Indentation Error at ${content.last.start}. Should end with a dedent token"))
    else
      val indents = content.count(_.isInstanceOf[Indent])
      val dedents = content.count(_.isInstanceOf[Dedent])
      if (indents != dedents) Failure(MacroError(s"Indentation Error at ${content.last.stop}"))
      else
        val duplicates = paramNames.groupBy(_.content).collect { case (x, seq @ Seq(_, _, _*)) => (x, seq.map(_.start)) }
        if (!duplicates.isEmpty)
          val (name, pos) = duplicates.head
          val msg         = s"The parameter `$name` is defined more than once at ${pos.map(_.toString).mkString(", ")}"
          Failure(MacroError(msg))
        else
          val parameterError = checkParams(content, paramNames)
          if (parameterError.isDefined)
            Failure(parameterError.get)
          else
            Success(Macro(name, paramNames.map(_.content), content.dropOnce.dropRightOnce))

  /** An undefined param is one is found */
  def checkParams(tokens: Seq[Token], paramNames: Seq[Literal]): Option[Exception] =
    tokens match
      case LeftChevron(_, _, _) :: Literal(param, _, pos, _) :: RightChevron(_, _, _) :: next =>
        if (!paramNames.exists(_.content == param))
          Some(ParameterError(s"parameter `$param` found at $pos is not defined in the macro def"))
        else
          checkParams(next, paramNames)
      case token :: next =>
        checkParams(next, paramNames)
      case Nil =>
        None
