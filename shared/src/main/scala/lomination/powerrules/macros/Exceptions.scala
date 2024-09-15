package lomination.powerrules.macros

class MacroError(message: String = "", cause: Throwable = null) extends Exception(message, cause)

class ParameterError(message: String = "", cause: Throwable = null) extends Exception(message, cause)
