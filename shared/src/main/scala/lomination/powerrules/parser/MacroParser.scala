package lomination.powerrules.parser

import scala.annotation.nowarn
import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.*
import scala.util.parsing.input.Reader
import org.log4s.getLogger
import lomination.powerrules.*

class MacroParser() extends RegexParsers {
  override protected val whiteSpace: Regex = "".r

  val logger = getLogger

  def apply(input: String, macroSeq: Seq[Macro] = Seq()): Try[String] =
    getMacro(macroSeq, input) match
      case scala.util.Success(newMacroSeq, remainder) => scala.util.Success(applyMacro(newMacroSeq, remainder))
      case scala.util.Failure(exception)              => scala.util.Failure(exception)

  def getMacro(macroSeq: Seq[Macro], remainder: String): Try[(Seq[Macro], String)] =
    parse(mac, applyMacro(macroSeq, remainder)) match
      case Success(result, next) => getMacro(macroSeq :+ result, next.source.toString.drop(next.offset))
      case Failure(msg, next)    => scala.util.Success((macroSeq, next.source.toString.drop(next.offset)))
      case Error(msg, next) =>
        val exception = IllegalArgumentException(msg)
        logger.error(exception)("Fail to parse macros (fatal error)")
        scala.util.Failure(exception)

  @nowarn // return of split cannot be empty
  def applyMacro(macroSeq: Seq[Macro], input: String): String =
    val macro1 = "^(\\w+)\\(([^)]*)\\)([\\S\\s]*)$".r // with params
    val macro2 = "^(\\w+)([\\S\\s]*)$".r              // without params
    input.split('$').toList match
      case head :: tail =>
        head + tail.foldRight("") { case (segment, acc) =>
          segment + acc match
            case macro1(name, params, remainder) =>
              macroSeq.find(_.name == name) match
                case Some(m) => m(params.split(',')).get + remainder
                case None =>
                  logger.warn(s"`$$` token is found but macro `$name` is not defined (yet)")
                  s"$$$name($params)" + remainder
            case macro2(name, remainder) =>
              macroSeq.find(_.name == name) match
                case Some(m) => m(Seq()).get + remainder
                case None =>
                  logger.warn(s"`$$` token is found but macro `$name` is not defined (yet)")
                  s"$$$name" + remainder
            case other =>
              logger.warn("`$` token is found but no macro call is detected")
              "$" + other
        }

  // macros
  type PreMacro = String ~ Option[Seq[String]] ~ String
  lazy val mac: Parser[Macro] = macDef ^^ { case name ~ params ~ content =>
    Macro(name, params.getOrElse(Seq()), content)
  }
  lazy val macDef: Parser[PreMacro]       = "(?:[ \n]*\n)?def +".r ~> macName ~ (macParams.? <~ " *=(?:[ \n]*\n| *)".r) ~ macContent
  lazy val macName: Parser[String]        = "\\w+".r
  lazy val macParams: Parser[Seq[String]] = "(" ~> "[ \\w,]+".r <~ ")" ^^ { _.split(',').toSeq.map(_.trim) }
  lazy val macContent: Parser[String]     = "\"" ~> "[^\"]+".r <~ "\""
}