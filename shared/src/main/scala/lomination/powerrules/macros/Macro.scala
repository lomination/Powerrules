package lomination.powerrules.macros

import lomination.powerrules.lexing.tokens.{Token, _}
import lomination.powerrules.util.style.{ansi0, ansi2, ansi4}
import lomination.powerrules.util.{dropOnce, dropRightOnce}

import scala.collection.mutable.Builder
import scala.util.parsing.input.{NoPosition, Position}
import scala.util.{Failure, Success, Try}

case class Macro(name: Literal, paramNames: Seq[String], content: Seq[Token]):

  val logger = org.log4s.getLogger

  def apply(paramValues: Seq[Seq[Token]], callStartPos: Position = NoPosition, callEndPos: Position = NoPosition): Try[Seq[Token]] =
    val valuesLen = paramValues.size
    val namesLen  = paramNames.size
    if (valuesLen != namesLen)
      val msg =
        s"Invalid given number of parameters for macro `${name.raw}` (defined at `${name.start}`) at $callStartPos (given: $valuesLen, expected: $namesLen)"
      val exception = ParameterError(msg)
      logger.error(exception)(msg)
      logger.debug(s"Params: $paramValues")
      Failure(exception)
    else
      val parameters = (paramNames zip paramValues).toMap
      replaceParameters(List.newBuilder, content.map(token => token.repos(callStartPos, callEndPos)).toList, parameters) // fixme

  // todo: make this function a parser
  def replaceParameters(computedOnes: Builder[Token, List[Token]], nextOnes: List[Token], parameters: Map[String, Seq[Token]]): Try[List[Token]] =
    nextOnes match
      case LeftChevron(_, _, _) :: Literal(param, pos, _) :: RightChevron(_, _, _) :: next =>
        parameters.get(param) match
          case Some(tokens) =>
            logger trace s"parameter $param successfully replaced at $ansi4$pos$ansi0 $ansi2(by `${tokens.map(_.raw).mkString}`)$ansi0 "
            replaceParameters(computedOnes.addAll(tokens), next, parameters)
          case None =>
            val e = MacroError(s"parameter $param not defined at $pos")
            logger.error(e)(s"Parameter $param at $ansi4$pos$ansi0 is not defined")
            Failure(e)
      case token :: next =>
        logger trace s"Neutral token ${token.getName} found and skipped at $ansi4${token.start}$ansi0"
        replaceParameters(computedOnes.addOne(token), next, parameters)
      case _ =>
        Success(computedOnes.result())

object Macro:

  def safeBuild(name: Literal, paramNames: Seq[Literal], content: Seq[Token]): Try[Macro] =
    if (!content.head.isInstanceOf[Indent]) Failure(MacroError(s"Indentation Error at ${content.head.start}. Should start with an indent token"))
    else if (!content.last.isInstanceOf[Dedent]) Failure(MacroError(s"Indentation Error at ${content.last.start}. Should end with a dedent token"))
    else
      val indents = content.count(_.isInstanceOf[Indent])
      val dedents = content.count(_.isInstanceOf[Dedent])
      if (indents != dedents) Failure(MacroError(s"Indentation Error at ${content.last.end}"))
      else
        val duplicates = paramNames.groupBy(_.raw).collect { case (x, seq @ Seq(_, _, _*)) => (x, seq.map(_.start)) }
        if (!duplicates.isEmpty)
          val (name, pos) = duplicates.head
          Failure(MacroError(s"The parameter `$name` is defined more than once at ${pos.map(_.toString).mkString(", ")}"))
        else Success(Macro(name, paramNames.map(_.raw), content.dropOnce.dropRightOnce))

  // Undefined parameters are not checked anymore. They are ignored.
  // /** An undefined param is one is found */
  // def checkParams(tokens: List[Token], paramNames: Seq[Literal]): Option[Exception] =
  //   tokens match
  //     case LeftChevron(_, _, _) :: Literal(param, pos, _) :: RightChevron(_, _, _) :: next =>
  //       if (!paramNames.exists(_.content == param))
  //         Some(ParameterError(s"parameter `$param` found at $pos is not defined in the macro def"))
  //       else
  //         checkParams(next, paramNames)
  //     case token :: next =>
  //       checkParams(next, paramNames)
  //     case _ =>
  //       None
