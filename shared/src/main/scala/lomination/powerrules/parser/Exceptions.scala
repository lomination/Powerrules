package lomination.powerrules.parser

case class ParsingError(message: String = "", cause: Throwable = null) extends Exception(message, cause)

case class InvalidNumberOfMacroParameters(message: String = "", cause: Throwable = null) extends Exception(message, cause)
