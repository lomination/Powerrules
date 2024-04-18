package lomination.ddnettools.parser

import scala.util.{Try, Success, Failure}
import org.log4s.getLogger
import lomination.ddnettools.RuleFile

case class MyParser():
  val logger = getLogger

  def apply(input: String): Try[RuleFile] =
    val macroParser    = MacroParser()
    val ruleFileParser = RuleFileParser()
    macroParser(preProcess(input)) match
      case Success(value) =>
        logger.info("Macro parser succeded")
        ruleFileParser(value)
      case Failure(exception) =>
        logger.error("Macro parser failed")
        Failure(exception)

  // replace `//` comments by empty strings
  def preProcess(input: String): String =
    input
      .replaceAll("//[^\n]*", "")
      .replaceAll("/\\*[\\S\\s]*\\*/", "")
