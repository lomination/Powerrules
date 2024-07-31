package lomination.powerrules.parsing

import scala.annotation.nowarn
import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.*

object MacroParser extends RegexParsers {
  override protected val whiteSpace: Regex = "".r

  val logger = org.log4s.getLogger

  /** Parser macros in the given input and return it after aplying them
    *
    * @param input
    * @param macroSeq
    * @return
    */
  def apply(input: String, macroSeq: Seq[Macro] = Seq()): String =
    getFirstMacro(applyMacros(macroSeq, input)) match
      case (scala.util.Success(m), remainder) => apply(remainder, Seq(m))
      case (scala.util.Failure(_), remainder) => remainder

  /** Parse the first found macro definition
    *
    * @param macroSeq
    * @param remainder
    * @return
    *   the found macro and the remaining input
    */
  def getFirstMacro(input: String): (Try[Macro], String) =
    parse(mac, input) match
      case Success(result, next) => (scala.util.Success(result), next.source.toString.drop(next.offset))
      case Failure(msg, next)    => (scala.util.Failure(ParsingError(msg)), input)
      case Error(msg, next)      => (scala.util.Failure(ParsingError(msg)), input)

  // def getMacro(macroSeq: Seq[Macro], remainder: String): (Seq[Macro], String) =
  //   parse(mac, applyMacro(macroSeq, remainder)) match
  //     case Success(result, next) => getMacro(macroSeq :+ result, next.source.toString.drop(next.offset))
  //     case Failure(msg, next)    => scala.util.Success((macroSeq, next.source.toString.drop(next.offset)))
  //     case Error(msg, next) =>
  //       val exception = IllegalArgumentException(msg)
  //       logger.error(exception)("Fail to parse macros (fatal error)")
  //       scala.util.Failure(exception)

  /** Search for every macro call and try to apply the given macros if they match
    *
    * @param macroSeq
    * @param input
    * @return
    */
  @nowarn // return of split cannot be empty
  def applyMacros(macroSeq: Seq[Macro], input: String): String =
    val withParams     = """^(\w+)\(([^)]*)\)([\S\s]*)$""".r
    val withoutParams1 = """^(\w+)([\S\s]*)$""".r
    val withoutParams2 = """^\{(\w+)}([\S\s]*)$""".r // with {}
    input.split('$').toList match
      case head :: tail =>
        head + tail.foldRight("") { case (segment, acc) =>
          segment + acc match
            case withParams(name, params, remainder) =>
              macroSeq.find(_.name == name) match
                case Some(m) => m(params.split(',')).get + remainder
                case None    => logger.warn(s"`$$` token is found but macro `$name` is not defined (yet?)"); s"$$$name($params)" + remainder
            case withoutParams1(name, remainder) =>
              macroSeq.find(_.name == name) match
                case Some(m) => m(Seq()).get + remainder
                case None    => logger.warn(s"`$$` token is found but macro `$name` is not defined (yet?)"); s"$$$name" + remainder
            case withoutParams2(name, remainder) =>
              macroSeq.find(_.name == name) match
                case Some(m) => m(Seq()).get + remainder
                case None    => logger.warn(s"`$$` token is found but macro `$name` is not defined (yet?)"); s"$$$name" + remainder
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

case class Macro(name: String, paramNames: Seq[String], content: String):
  // if (paramNames == Seq(""))
  //   logger.warn(
  //     s"Macro $name is defined with one parameter named with empty string ``.\n" +
  //       "Consider not using parentheses after its definition if no paramter is expected."
  //   )
  if (paramNames.sizeIs != paramNames.toSet.size)
    logger.warn(s"Some parameters of macro $name have the same name.")
  val logger = org.log4s.getLogger
  def apply(paramValues: Seq[String]): Try[String] =
    val vLength = paramValues.size
    val nLength = paramNames.size
    if (vLength != nLength)
      val msg       = s"Invalid number of params for macro `$name`. Given: $vLength, expected: $nLength"
      val exception = InvalidNumberOfMacroParameters(msg)
      logger.error(msg)
      logger.debug(s"Given params: `${paramValues.mkString("`, `")}`")
      scala.util.Failure(exception)
    else
      val res = (paramNames zip paramValues)
        .foldLeft(content) { case (c, (n, v)) => c.replace(s"<$n>", v) }
      scala.util.Success(res)
