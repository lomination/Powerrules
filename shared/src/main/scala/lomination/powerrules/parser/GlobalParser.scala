package lomination.powerrules.parser

import scala.util.{Try, Success, Failure}
import lomination.powerrules.RuleFile

object GlobalParser:
  val logger = org.log4s.getLogger

  def apply(input: String): Try[RuleFile] =
    MacroParser(preProcess(input)) match
      case Success(value) =>
        logger.info("Macro parser succeded")
        RuleFileParser(value)
      case Failure(exception) =>
        logger.error("Macro parser failed")
        Failure(exception)

  // replace `//` comments by empty strings
  def preProcess(input: String): String =
    input
      .replaceAll("/\\*[\\S\\s]*\\*/", "")
      .replaceAll("//[^\n]*", "")
