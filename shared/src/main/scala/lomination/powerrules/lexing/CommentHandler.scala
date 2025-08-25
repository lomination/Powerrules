package lomination.powerrules.lexing

import lomination.powerrules.lexing.tokens._

import scala.util.Try

object CommentHandler extends TokenParser {
  def apply(tokens: Seq[Token]): Try[Seq[Token]] =
    parse(mainParser, TokenReader(tokens)) match
      case Success(tokens, _)     => scala.util.Success(tokens)
      case NoSuccess.I(msg, next) => scala.util.Failure(Exception(msg + s" at ${next.pos}"))

  lazy val mainParser: P[Seq[Token]] =
    rep(singleLineComment | multiLineComment | anyTk)
      ^^ { tokens => tokens collect { case tk: Token => tk } }

  lazy val singleLineComment: P[Unit] =
    slashTk ~ slashTk ~ rep(not(newlineTk) ~ anyTk) ~ opt(newlineTk) ^^^ { () }

  lazy val multiLineComment: P[Unit] =
    slashTk ~ starTk ~ rep(not(starTk ~ slashTk) ~ anyTk) ~ starTk ~ slashTk ^^^ { () }
}
