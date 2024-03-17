package lomination.ddnettools.parser

import scala.util.parsing.combinator.*
import scala.util.matching.Regex
import scala.util.Try
import lomination.ddnettools.*

class MyParser extends RegexParsers {
  override protected val whiteSpace: Regex = "".r

  def apply(input: String): Try[Autorule] = parseAll(autorule, input) match
    case Success(result, next) => scala.util.Success(result)
    case Error(msg, next)      => scala.util.Failure(new IllegalArgumentException(msg))
    case Failure(msg, next)    => scala.util.Failure(new IllegalArgumentException(msg))

  // regex
  val anyWS: Regex   = "[\n ]*".r   // any white space character (new line or space)
  val anyWSNL: Regex = "[\n ]*\n".r // any white space followed by a new line
  val ws: Regex      = " *".r       // space
  val nL: Regex      = "\n".r       // new line
  val ind: Regex     = "  ".r       // indent
  val ind2: Regex    = "    ".r     // indent twice
  val eof: Regex     = "$".r        // end of file

  // general
  def autorule: Parser[Autorule] = anyWS ~> (tile <~ anyWSNL) ~ repsep(rule, anyWS) <~ anyWS ^^ { case t ~ r =>
    Autorule(DefaultTile(t.id, t.dir), r)
  }
  def rule: Parser[Rule] = (ruleName <~ anyWSNL) ~ repsep(command, anyWS) ^^ { case n ~ c =>
    Rule(n, c)
  }
  def ruleName: Parser[String] = "[" ~> "\\w+".r <~ "]"

  // commands
  def command: Parser[Command] = replace | shadow // | shape
  def replace: Parser[Replace] = replaceKW ~> tiles ~ rep(when) ~ random.? ~ autorotate.? <~ endreplaceKW
    ^^ { case t ~ c ~ r ~ a => Replace(t(0), c.flatten, r.getOrElse(Random.always), a.getOrElse(Seq(Dir.default))) }
  def shadow: Parser[Shadow] = shadowKW ~> tiles ~ when.? ~ softdiag.? <~ endshadowKW
    ^^ { case t ~ c ~ d => Shadow(t, c.getOrElse(Seq(Pos(0, 0) is FullMatcher)), d.getOrElse(false)) }

  // command keywords
  def replaceKW: Parser[Unit]    = "replace" ~ anyWSNL                           ^^ { _ => () }
  def endreplaceKW: Parser[Unit] = (anyWS ~ "endreplace" ~ (anyWSNL | ws ~ eof)) ^^ { _ => () }
  def shadowKW: Parser[Unit]     = "shadow" ~ anyWSNL                            ^^ { _ => () }
  def endshadowKW: Parser[Unit]  = (anyWS ~ "endshadow" ~ (anyWSNL | ws ~ eof))  ^^ { _ => () }

  // statements
  def tiles: Parser[Seq[Tile]]     = tileKW ~> rep1sep(tile, (anyWSNL ~ ind2) | " +".r) <~ anyWSNL
  def when: Parser[Seq[Cond]]      = whenKW ~> rep1sep(condition, anyWSNL ~ ind2) <~ anyWSNL
  def random: Parser[Random]       = randomKW ~> randomChance <~ anyWSNL
  def autorotate: Parser[Seq[Dir]] = autorotateKW ~> autorotateDirs <~ anyWSNL
  def softdiag: Parser[Boolean]    = softdiagKW <~ anyWSNL ^^ { _ => true }

  // statement keywords
  def tileKW: Parser[Unit]       = (ind ~ "tile" ~ ((anyWSNL ~ ind2) | ws))          ^^ { _ => () }
  def whenKW: Parser[Unit]       = (ind ~ "when" ~ ((anyWSNL ~ ind2) | ws))          ^^ { _ => () }
  def randomKW: Parser[Unit]     = (ind ~ "random" ~ ((anyWSNL ~ ind2) | ws))        ^^ { _ => () }
  def autorotateKW: Parser[Unit] = (ind ~ "autorotate" ~ ((anyWSNL ~ ind2) | ws))    ^^ { _ => () }
  def softdiagKW: Parser[Unit]   = (ind ~ "softdiagonals" ~ ((anyWSNL ~ ind2) | ws)) ^^ { _ => () }

  // others
  def condition: Parser[Cond] =
    (pos <~ ws) ~ matcher ^^ { case p ~ m =>
      Cond(p, m)
    }

  def pos: Parser[Pos] =
    (("\\( ?".r ~> ("-?\\d+".r <~ " ?, ?".r.?) ~ "-?\\d+".r <~ " ?\\)".r) | (("-?\\d+".r <~ " +".r) ~ "-?\\d+".r)) ^^ { case x ~ y =>
      Pos(x.toInt, y.toInt)
    }

  def operator: Parser[Operator] =
    "==|!=|is|is ?not".r ^^ { o =>
      if (o == "==" || o == "is") Operator.Equal else Operator.NotEqual
    }

  def matcher: Parser[Matcher] =
    fullMatcher | emptyMatcher | genericMatcher

  def fullMatcher: Parser[FullMatcher.type] =
    "full" ^^ { _ => FullMatcher }

  def emptyMatcher: Parser[EmptyMatcher.type] =
    "empty" ^^ { _ => EmptyMatcher }

  def genericMatcher: Parser[GenericMatcher] =
    (operator <~ ws) ~ rep1sep(tileMatcher, " " ~ ws) ^^ { case op ~ tms => GenericMatcher(op, tms*) }

  def tileMatcher: Parser[TileMatcher] =
    id ~ (dir | anyDir) ^^ { case i ~ d => TileMatcher(i, d) }

  def randomChance: Parser[Random] =
    "\\d+(?:\\.\\d*)?".r ~ "%?".r ^^ { case n ~ p =>
      if (p == "%") Random(n.toFloat) else Random(n.toFloat * 100)
    }

  def autorotateDirs: Parser[Seq[Dir]] =
    repsep(dir, ws | (anyWSNL ~ ind2))

  def tile: Parser[Tile] =
    id ~ dir.? ^^ { case i ~ d => Tile(i, d.getOrElse(Dir.default)) }

  def id: Parser[Int] =
    "[a-f0-9]{1,2}".r ^^ { i => Integer.parseInt(i, 16) }

  def dir: Parser[Dir] =
    "[+-]".r ~ "[0-3]".r ^^ { case s ~ t =>
      Dir(if (s == "+") Sign.+ else Sign.-, Times.fromOrdinal(t.toInt))
    }

  def anyDir: Parser[AnyDir.type] =
    "*" ^^ { _ => AnyDir }
}
