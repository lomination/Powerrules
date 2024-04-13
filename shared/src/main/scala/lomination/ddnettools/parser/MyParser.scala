package lomination.ddnettools.parser

import scala.util.{Try, Success, Failure}
import lomination.ddnettools.RuleFile

case class MyParser():
  def apply(input: String): Try[RuleFile] =
    val macroParser = MacroParser()
    val ruleFileParser = RuleFileParser()
    macroParser(input) match
      case Success(value) => ruleFileParser(value)
      case Failure(exception) => Failure(exception)

  // replace `//` comments by empty strings
  def preProcess(input: String): String =
    input
      .replaceAll("//[^\n]*", "")
      .replaceAll("/*[\\S\\s]*/", "")
