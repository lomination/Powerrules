package lomination.powerrules.formatting

import lomination.powerrules.util.style.{ansi0, ansi2, ansi4, ansi31}
import lomination.powerrules.util.dropOnce
import lomination.powerrules.lexing.tokens.{Token, Newline, Dedent, Indent, Space}
import lomination.powerrules.config.Config
import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}
import scala.util.parsing.input.{Position, NoPosition}

object Formatter {

  val logger = org.log4s.getLogger

  /** Processes indentation */
  def apply(tokens: Seq[Token])(using Config): Try[Seq[Token]] =
    if (tokens.isEmpty)
      logger trace "Empty token list given"
      Success(Seq())
    else
      processIndentation(Seq(), tokens, 0)

  @tailrec
  def processIndentation(cookedOnes: Seq[Token], rawOnes: Seq[Token], indentLevel: Int)(using config: Config): Try[Seq[Token]] =
    rawOnes match
      case (newline @ Newline(raw, start, stop)) :: rest =>
        val (indentation, next) = rest.span(_.isInstanceOf[Space])
        if (next.isEmpty || next.head.isInstanceOf[Newline | Indent | Dedent])
          logger trace s"$ansi4$start$ansi0: Newline token has been found and skipped"
          processIndentation(cookedOnes, next, indentLevel)
        else
          val spaces = indentation.size
          if (spaces % config.pIndentation != 0)
            val e = IndentationError(s"Invalid indentation (non divisible by ${config.pIndentation}) at $start")
            logger.error(e)(s"$ansi31$ansi4$start$ansi0: Newline token has been found with invalid indentation")
            Failure(e)
          else
            val newLevel = spaces / config.pIndentation
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
        processIndentation(cookedOnes, rawOnes.dropOnce, indentLevel)
      case (token @ Token(_, start, _)) :: next =>
        logger trace s"$ansi4$start$ansi0: Neutral token (${token.getName}) has been found"
        processIndentation(cookedOnes :+ token, next, indentLevel)
      case Nil =>
        logger trace s"End of source has been reached"
        val lastPos = cookedOnes.lastOption.map(_.stop).getOrElse(NoPosition)
        Success(
          cookedOnes ++ List.fill(indentLevel)(Dedent(lastPos, lastPos))
        )

}
