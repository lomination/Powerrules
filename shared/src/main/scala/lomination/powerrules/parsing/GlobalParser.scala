package lomination.powerrules.parsing

import scala.util.{Try, Success, Failure}
import lomination.powerrules.RuleFile

object GlobalParser:
  val logger = org.log4s.getLogger

  def apply(input: String): Try[RuleFile] =
    RuleFileParser(MacroParser(preProcess(input)))

  // replace `//` comments by empty strings
  def preProcess(input: String): String =
    input
      .replaceAll("/\\*[\\S\\s]*\\*/", "")
      .replaceAll("//[^\n]*", "")
