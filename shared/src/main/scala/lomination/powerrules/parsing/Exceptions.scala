package lomination.powerrules.parsing

class ParsingError(message: String = "", cause: Throwable = null) extends Exception(message, cause)

class InvalidNumberOfMacroParameters(message: String = "", cause: Throwable = null) extends Exception(message, cause)
