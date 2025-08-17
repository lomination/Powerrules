package lomination.powerrules.formatting

import lomination.powerrules.util.style.{ansi0, ansi2, ansi31, ansi4}
import lomination.powerrules.util.dropOnce
import lomination.powerrules.lexing.tokens.{Dedent, Indent, Newline, Space, Tab, Token}
import lomination.powerrules.config.Config
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import scala.util.parsing.input.{NoPosition, Position}

object Formatter {

  val logger = org.log4s.getLogger

  /** Processes indentation */
  def apply(tokens: Seq[Token])(using Config): Try[Seq[Token]] =
    if (tokens.isEmpty)
      logger trace "Empty token list given"
      Success(Seq())
    else processIndentation(Seq(), tokens.toList, 0)

  @tailrec
  def processIndentation(cookedOnes: Seq[Token], rawOnes: List[Token], indentLevel: Int)(using config: Config): Try[Seq[Token]] =
    rawOnes match
      case (newline @ Newline(raw, start, stop)) :: rest =>
        val (indentation, next) = if config.pUseTabs then rest.span(_.isInstanceOf[Tab]) else rest.span(_.isInstanceOf[Space])
        if (next.isEmpty || next.head.isInstanceOf[Newline | Indent | Dedent])
          logger trace s"$ansi4$start$ansi0: Newline token has been found and skipped"
          processIndentation(cookedOnes, next, indentLevel)
        else
          val spaces = indentation.size
          if (!config.pUseTabs && spaces % config.pIndentation != 0)
            val e = IndentationError(s"Invalid indentation (non divisible by ${config.pIndentation}) at $start")
            logger.error(e)(s"$ansi4$start$ansi0: Newline token has been found with invalid indentation")
            Failure(e)
          else
            val newLevel = if config.pUseTabs then spaces else spaces / config.pIndentation
            if (newLevel == indentLevel)
              logger trace s"$ansi4$start$ansi0: Newline token has been found"
              processIndentation(cookedOnes :+ newline, next, newLevel)
            else if (newLevel > indentLevel)
              logger trace s"$ansi4$start$ansi0: Newline token has been found (indenation level has been increased)"
              val c = cookedOnes ++ List.fill(newLevel - indentLevel)(Indent(start, stop))
              processIndentation(c, next, newLevel)
            else
              logger trace s"$ansi4$start$ansi0: Newline token has been found (indenation level has been decreased)"
              val c = cookedOnes ++ List.fill(indentLevel - newLevel)(Dedent(start, stop))
              processIndentation(c, next, newLevel)
      case (space @ Space(_, start, _)) :: (_: (Space | Newline)) :: _ =>
        logger trace s"$ansi4$start$ansi0: Space token has been found and skipped"
        processIndentation(cookedOnes, rawOnes.drop(1), indentLevel)
      case (token @ Token(_, start, _)) :: next =>
        logger trace s"$ansi4$start$ansi0: Neutral token (${token.getName}) has been found"
        processIndentation(cookedOnes :+ token, next, indentLevel)
      // todo: what is this case needed?
      case head :: next => 
        throw Exception(s"This case should not be possible.\nHead: $head;\nNext: $next;")
      case Nil =>
        logger trace s"End of source has been reached"
        val lastPos = cookedOnes.lastOption.map(_.stop).getOrElse(NoPosition)
        Success(
          cookedOnes ++ List.fill(indentLevel)(Dedent(lastPos, lastPos))
        )

}
