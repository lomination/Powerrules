package lomination.powerrules.lexing

import lomination.powerrules.lexing.tokens._

import scala.util.Try

object CommentHandler extends TokenParser {

  /** Removes all comments (single line and multi line comments) from the given token sequence. Note that single line comments do not include the new
    * line that determines their end.
    *
    * @param tokens
    *   the input token sequence.
    * @return
    *   a token sequence where all comments have been removed.
    */
  def apply(tokens: Seq[Token]): Try[Seq[Token]] =
    logger.trace("Starting comment handling...")
    parse(mainParser, TokenReader(tokens)) match
      case Success(tokens, _) =>
        logger.trace("Comment handling succeeded")
        scala.util.Success(tokens)
      case NoSuccess.I(msg, next) =>
        logger.trace(s"Comment handling failed at ${next.pos} due to:\n$msg")
        scala.util.Failure(Exception(msg + s" at ${next.pos}"))

  lazy val mainParser: P[Seq[Token]] =
    rep(singleLineComment | multiLineComment | anyTk)
      ^^ { tokens => tokens collect { case tk: Token => tk } }

  lazy val singleLineComment: P[Unit] =
    slashTk ~ slashTk ~! rep(not(newlineTk) ~ anyTk) ^^^ { () }
      |< "single line comment"

  lazy val multiLineComment: P[Unit] =
    slashTk ~ starTk ~! rep(not(starTk ~ slashTk) ~ anyTk) ~ (starTk ~ slashTk |< "multi line comment end `*/`") ^^^ { () }
      |< "multi line comment"
}
