package lomination.ddnettools.parser

import scala.util.parsing.combinator.*
import scala.util.matching.Regex
import lomination.ddnettools.*
import scala.util.Try

class MyParser extends RegexParsers {
  override protected val whiteSpace: Regex = "".r
  def apply(input: String): Try[Autorule] = parseAll(autorule, input) match
    case Success(result, next) => scala.util.Success(result)
    case Error(msg, next)      => sys.error(msg)
    case Failure(msg, next)    => sys.error(msg)
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
  def command: Parser[Command] = reset | replace | shadow // | shape
  def reset: Parser[Reset]     = resetKW ~> tileS ~ randomS.? ~ autorotateS.? ~ noDefRuleS.? <~ endresetKW                 ^^ { case t ~ r ~ a ~ dr => Reset(t(0), r.getOrElse(Random.always), a.getOrElse(Seq(Dir.default)), dr.getOrElse(false)) }
  def replace: Parser[Replace] = replaceKW ~> tileS ~ rep1(ifS) ~ randomS.? ~ autorotateS.? ~ noDefRuleS.? <~ endreplaceKW ^^ { case t ~ c ~ r ~ a ~ dr => Replace(t(0), c.flatten, r.getOrElse(Random.always), a.getOrElse(Seq(Dir.default)), dr.getOrElse(false)) }
  def shadow: Parser[Shadow]   = shadowKW ~> tileS ~ softdiagS.? <~ endshadowKW                                            ^^ { case l ~ d => Shadow(l, d.getOrElse(false)) }
  // command keywords
  def resetKW: Parser[Unit]      = "reset" ~ anyWSNL                             ^^ { _ => () }
  def endresetKW: Parser[Unit]   = (anyWS ~ "endreset" ~ (anyWSNL | sp ~ eof))   ^^ { _ => () }
  def replaceKW: Parser[Unit]    = "replace" ~ anyWSNL                           ^^ { _ => () }
  def endreplaceKW: Parser[Unit] = (anyWS ~ "endreplace" ~ (anyWSNL | sp ~ eof)) ^^ { _ => () }
  def shadowKW: Parser[Unit]     = "shadow" ~ anyWSNL                            ^^ { _ => () }
  def endshadowKW: Parser[Unit]  = (anyWS ~ "endshadow" ~ (anyWSNL | sp ~ eof))  ^^ { _ => () }
  // statements
  def tileS: Parser[Seq[Tile]]      = tileKW ~> rep1sep(tile, anyWSNL ~ ind2) <~ anyWSNL
  def ifS: Parser[Seq[Cond]]        = ifKW ~> repsep(condition, anyWSNL ~ ind2) <~ anyWSNL
  def randomS: Parser[Random]       = randomKW ~> randomChance <~ anyWSNL
  def autorotateS: Parser[Seq[Dir]] = autorotateKW ~> autorotateDirs <~ anyWSNL
  def softdiagS: Parser[Boolean]    = softdiagKW <~ anyWSNL  ^^ { _ => true }
  def noDefRuleS: Parser[Boolean]   = nodefruleKW <~ anyWSNL ^^ { _ => true }
  // statement keywords
  def tileKW: Parser[Unit]       = (ind ~ "tile" ~ ((anyWSNL ~ ind2) | sp))          ^^ { _ => () }
  def ifKW: Parser[Unit]         = (ind ~ "if" ~ ((anyWSNL ~ ind2) | sp))            ^^ { _ => () }
  def randomKW: Parser[Unit]     = (ind ~ "random" ~ ((anyWSNL ~ ind2) | sp))        ^^ { _ => () }
  def autorotateKW: Parser[Unit] = (ind ~ "autorotate" ~ ((anyWSNL ~ ind2) | sp))    ^^ { _ => () }
  def softdiagKW: Parser[Unit]   = (ind ~ "softdiagonals" ~ ((anyWSNL ~ ind2) | sp)) ^^ { _ => () }
  def nodefruleKW: Parser[Unit]  = (ind ~ "nodefaultrule" ~ ((anyWSNL ~ ind2) | sp)) ^^ { _ => () }
  // others
  def condition: Parser[Cond]          = (pos <~ sp) ~ (operator <~ sp) ~ repsep(tileMatcher, sp ~ "or".? ~ sp) ^^ { case p ~ o ~ t => Cond(p, o, t: _*) }
  def pos: Parser[Pos]                 = "(" ~ sp ~> ("-?\\d+".r <~ sp ~ "," ~ sp) ~ "-?\\d+".r <~ sp ~ ")"     ^^ { case x ~ y => Pos(x.toInt, y.toInt) }
  def operator: Parser[Operator]       = "==|!=|is|is ?not".r                                                   ^^ { o => if (o == "==" || o == "is") Operator.Equal else Operator.NotEqual }
  def tileMatcher: Parser[TileMatcher] = fullTM | emptyTM | genericTM
  def fullTM: Parser[TileMatcher]      = "full"                                                                 ^^ { case _ => TileMatcher(FullTile, AnyDir) }
  def emptyTM: Parser[TileMatcher]     = "empty"                                                                ^^ { case _ => TileMatcher(0, AnyDir) }
  def genericTM: Parser[TileMatcher]   = id ~ (dir | anyDir)                                                    ^^ { case i ~ d => TileMatcher(i, d) }
  def randomChance: Parser[Random]     = "\\d+(?:\\.\\d*)?".r ~ "%?".r                                          ^^ { case n ~ p => if (p == "%") Random(n.toFloat) else Random(n.toFloat * 100) }
  def autorotateDirs: Parser[Seq[Dir]] = repsep(dir, sp | (anyWSNL ~ ind2))
  def tile: Parser[Tile]               = id ~ dir                                                               ^^ { case i ~ d => Tile(i, d) }
  def id: Parser[Int]                  = "[a-f0-9]{1,2}".r                                                      ^^ { i => Integer.parseInt(i, 16) }
  def dir: Parser[Dir]                 = "[+-]".r ~ "[0-3]".r                                                   ^^ { case s ~ t => Dir(if (s == "+") Sign.+ else Sign.-, Times.fromOrdinal(t.toInt)) }
  def anyDir: Parser[AnyDir.type]      = "*"                                                                    ^^ { _ => AnyDir }
}
