package lomination.ddnettools.parser

import scala.util.parsing.combinator.*
import scala.util.matching.Regex
import lomination.ddnettools.*

class AutoruleParser extends RegexParsers {
  override protected val whiteSpace: Regex = "".r
  def apply(input: String) = parse(autorule, input)
  // regex
  val anyWS: Regex   = "[\n ]*".r   // any white space
  val anyWSNL: Regex = "[\n ]*\n".r // any white space followed by new line
  val sp: Regex      = " *".r       // space
  val nL: Regex      = "\n".r       // new line
  val ind: Regex     = "  ".r       // indent
  val ind2: Regex    = "    ".r     // indent twice
  val eof: Regex     = "$".r        // end of file
  // general
  def autorule: Parser[Autorule] = anyWS ~> repsep(rule, anyWS) <~ anyWS ^^ { Autorule(_) }
  def rule: Parser[Rule]         = ruleName ~ repsep(command, anyWS)     ^^ { case n ~ c => Rule(n, c) }
  def ruleName: Parser[String]   = "[" ~> "\\w+".r <~ "]" ~ anyWSNL
  // commands
  def command: Parser[Command] = clear | reset | replace | shadow // | shape
  def clear: Parser[Clear]     = clearKW ~> withS ~ randomS.? ~ autorotateS.? <~ endclearKW                 ^^ { case t ~ r ~ a => Clear(t(1), r.getOrElse(100f), a.getOrElse(Seq())) }
  def reset: Parser[Reset]     = resetKW ~> withS ~ randomS.? ~ autorotateS.? <~ endresetKW                 ^^ { case t ~ r ~ a => Reset(t(1), r.getOrElse(100f), a.getOrElse(Seq())) }
  def replace: Parser[Replace] = replaceKW ~> withS ~ rep1(ifS) ~ randomS.? ~ autorotateS.? <~ endreplaceKW ^^ { case t ~ c ~ r ~ a => Replace(t(1), c.flatten, r.getOrElse(100f), a.getOrElse(Seq())) }
  def shadow: Parser[Shadow]   = shadowKW ~> withS ~ softdiagS.? <~ endshadowKW                             ^^ { case l ~ d => Shadow(l, d.getOrElse(false)) }
  // command keywords
  def clearKW: Parser[Unit]      = "clear" ~ anyWSNL                             ^^ { _ => () }
  def endclearKW: Parser[Unit]   = (anyWS ~ "endclear" ~ (anyWSNL | sp ~ eof))   ^^ { _ => () }
  def resetKW: Parser[Unit]      = "reset" ~ anyWSNL                             ^^ { _ => () }
  def endresetKW: Parser[Unit]   = (anyWS ~ "endreset" ~ (anyWSNL | sp ~ eof))   ^^ { _ => () }
  def replaceKW: Parser[Unit]    = "replace" ~ anyWSNL                           ^^ { _ => () }
  def endreplaceKW: Parser[Unit] = (anyWS ~ "endreplace" ~ (anyWSNL | sp ~ eof)) ^^ { _ => () }
  def shadowKW: Parser[Unit]     = "shadow" ~ anyWSNL                            ^^ { _ => () }
  def endshadowKW: Parser[Unit]  = (anyWS ~ "endshadow" ~ (anyWSNL | sp ~ eof))  ^^ { _ => () }
  // statements
  def withS: Parser[Seq[Tile]]      = withKW ~> rep1sep(tile, anyWSNL ~ ind2) <~ anyWSNL
  def ifS: Parser[Seq[Cond]]   = ifKW ~> repsep(condition, anyWSNL ~ ind2) <~ anyWSNL
  def randomS: Parser[Float]        = randomKW ~> randomChance <~ anyWSNL
  def autorotateS: Parser[Seq[Dir]] = autorotateKW ~> autorotateDirs <~ anyWSNL
  def softdiagS: Parser[Boolean]    = softdiagKW ~> "true|yes|false|no".r <~ anyWSNL ^^ { b => if (b == "true" || b == "yes") true else false }
  // statement keywords
  def withKW: Parser[Unit]       = (ind ~ "with" ~ ((anyWSNL ~ ind2) | sp))          ^^ { _ => () }
  def ifKW: Parser[Unit]         = (ind ~ "if" ~ ((anyWSNL ~ ind2) | sp))            ^^ { _ => () }
  def ofKW: Parser[Unit]         = (ind ~ "of" ~ ((anyWSNL ~ ind2) | sp))            ^^ { _ => () }
  def randomKW: Parser[Unit]     = (ind ~ "random" ~ ((anyWSNL ~ ind2) | sp))        ^^ { _ => () }
  def autorotateKW: Parser[Unit] = (ind ~ "autorotate" ~ ((anyWSNL ~ ind2) | sp))    ^^ { _ => () }
  def softdiagKW: Parser[Unit]   = (ind ~ "softdiagonals" ~ ((anyWSNL ~ ind2) | sp)) ^^ { _ => () }
  // others
  def condition: Parser[Cond]     = (pos <~ sp) ~ (operator <~ sp) ~ repsep(tileMatcher, sp ~ "or".? ~ sp) ^^ { case p ~ o ~ t => Cond(p, o, t: _*) }
  def pos: Parser[Pos]                 = "(" ~ sp ~> ("-?\\d+".r <~ sp ~ "," ~ sp) ~ "-?\\d+".r <~ sp ~ ")"     ^^ { case x ~ y => Pos(x.toInt, y.toInt) }
  def operator: Parser[Operator]       = "==|!=|is|is ?not".r                                                   ^^ { o => if (o == "==" || o == "is") Operator.Equal else Operator.NotEqual }
  def tileMatcher: Parser[TileMatcher] = fullTM | emptyTM | genericTM
  def fullTM: Parser[TileMatcher]      = "full"                                                                 ^^ { case _ => TileMatcher(FullTile, AnyDir) }
  def emptyTM: Parser[TileMatcher]     = "empty"                                                                ^^ { case _ => TileMatcher(0, AnyDir) }
  def genericTM: Parser[TileMatcher]   = id ~ (dir | anyDir)                                                    ^^ { case i ~ d => TileMatcher(i, d) }
  def randomChance: Parser[Float]      = "\\d+(?:\\.\\d*)?".r ~ "%?".r                                          ^^ { case n ~ p => if (p == "%") n.toFloat else n.toFloat / 100 }
  def autorotateDirs: Parser[Seq[Dir]] = repsep(dir, sp | (anyWSNL ~ ind2))
  def tile: Parser[Tile]               = id ~ dir                                                               ^^ { case i ~ d => Tile(i, d) }
  def id: Parser[Int]                  = "[a-f0-9]{1,2}".r                                                      ^^ { i => Integer.parseInt(i, 16) }
  def dir: Parser[Dir]                 = "[+-]".r ~ "[0-3]".r                                                   ^^ { case s ~ t => Dir(if (s == "+") Sign.Plus else Sign.Minus, Times.fromOrdinal(t.toInt)) }
  def anyDir: Parser[AnyDir.type]      = "*"                                                                    ^^ { _ => AnyDir }
}
