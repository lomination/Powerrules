package lomination.powerrules.lexing

class SegmentationError(message: String = "", cause: Throwable = null) extends Exception(message, cause)

class TokenizationError(message: String = "", cause: Throwable = null) extends Exception(message, cause)

class CommentError(message: String = "", cause: Throwable = null) extends Exception(message, cause)

class IndentationError(message: String = "", cause: Throwable = null) extends Exception(message, cause)

class LexingError(message: String = "", cause: Throwable = null) extends Exception(message, cause)
