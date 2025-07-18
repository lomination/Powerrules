package lomination.powerrules.parsing

/** Exception thrown when an unknown token (class Unknown from lomination.powerrules.lexing.tokens) is encountered while parsing Powerrules.
  */
class TokenParsingError(message: String = "", cause: Throwable = null) extends Exception(message, cause)
