package lomination.ddnettools.parser

import scala.util.parsing.combinator.*
import scala.util.matching.Regex
import scala.util.Try
import lomination.ddnettools.*

class MyParser extends RegexParsers {
  override protected val whiteSpace: Regex = "".r

  var macros: Map[String, (Seq[String], String)] = Map()

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
  def autorule: Parser[Autorule] =
    anyWS ~> (defaultTile <~ anyWSNL) ~ /* repsep(macroDef, anyWS) ~ */ repsep(rule, anyWS) <~ anyWS ^^ { case t ~ r => Autorule(t, r) }
  def defaultTile: Parser[DefaultTile] =
    "defaultTile += +".r ~> id ~ dir ^^ { case id ~ dir => DefaultTile(id, dir) }
  def rule: Parser[Rule] =
    (ruleName <~ anyWSNL) ~ repsep(command, anyWS) ^^ { case n ~ c => Rule(n, c) }
  def ruleName: Parser[String] = "[" ~> "[ \\w\\p{Punct}&&[^\\[\\]]]+".r <~ "]"

  // macros
  def macroDef: Parser[Unit] = "def +".r ~> macroName ~ (macroParams <~ ws ~ "=" ~ ws) ~ macroContent <~ anyWSNL ^^ { case n ~ p ~ c =>
    macros = macros + (n -> (p, c))
  }
  def macroName: Parser[String] = "\\w+".r
  def macroParams: Parser[Seq[String]] = "(" ~ ws ~> repsep(macroParam, ws ~ "," ~ ws) <~ ws ~ ")"
  def macroContent: Parser[String] = "(\n" ~> ".+" <~ "\n)"
  def macroParam: Parser[String] = "\\w+".r

  // commands
  def command: Parser[Command] = replace // | shadow | comment // | shape
  def replace: Parser[Replace] = replaceKw ~> tiS ~ rep(wS) ~ rS.? ~ arS.? <~ endreplaceKw
    ^^ { case t ~ c ~ r ~ a => Replace(t(0), c.flatten, r.getOrElse(Random.always), a.getOrElse(Seq(Dir.default))) }
  // def shadow: Parser[Shadow] = shadowKw ~> tiS ~ wS.? ~ stS.? <~ endshadowKw
    // ^^ { case t ~ c ~ st => Shadow(t, c.getOrElse(Seq(Pos(0, 0) is FullMatcher)), st.getOrElse(ShadowType.Default)) }
  def comment: Parser[Comment] = ("#" | "//") ~> "[ \\w\\p{Punct}]+".r <~ anyWSNL
    ^^ { case str => Comment(str) }

  // command keywords
  def cmdKw(name: Parser[?]): Parser[Unit]    = name ~ anyWSNL ^^ { _ => () }
  def endCmdKw(name: Parser[?]): Parser[Unit] = (anyWS ~ name ~ (anyWSNL | ws ~ eof)) ^^ { _ => () }
  def replaceKw: Parser[Unit]    = cmdKw("replace" | "re")
  def endreplaceKw: Parser[Unit] = endCmdKw("endreplace")
  def shadowKw: Parser[Unit]     = cmdKw("shadow" | "sd")
  def endshadowKw: Parser[Unit]  = endCmdKw("endshadow")

  // statements
  def tiS: Parser[Seq[Tile]]       = tileKw ~> rep1sep(tile, (anyWSNL ~ ind2) | " +".r) <~ anyWSNL
  def wS: Parser[Seq[Cond]]        = whenKw ~> rep1sep(condition, anyWSNL ~ ind2) <~ anyWSNL
  // def stS: Parser[ShadowType] = shadowTypeKw ~> noCst | defST | sDST <~ anyWSNL
  def rS: Parser[Random]         = randomKw ~> randomChance <~ anyWSNL
  def arS: Parser[Seq[Dir]]   = autorotateKw ~> repsep(dir, " +".r | (anyWSNL ~ ind2)) <~ anyWSNL
  def sdS: Parser[Boolean]      = softdiagKw ~ anyWSNL ^^ { _ => true }

  // statement keywords
  def sKw(name: Parser[_]): Parser[Unit] = ind ~ name ~ ((anyWSNL ~ ind2) | " +".r) ^^ { _ => () }
  def tileKw: Parser[Unit]       = sKw("tiles?".r | "with")
  def whenKw: Parser[Unit]       = sKw("when" | "if")
  def shadowTypeKw: Parser[Unit] = sKw("type")
  def randomKw: Parser[Unit]     = sKw("random")
  def autorotateKw: Parser[Unit] = sKw("autorotate")
  def softdiagKw: Parser[Unit]   = ind ~ "softdiagonals" ~ anyWSNL ^^ { _ => () }

  // others
  def condition: Parser[Cond] =
    (pos <~ ws) ~ matcher ^^ { case p ~ m =>
      Cond(p, m)
    }

  def pos: Parser[Pos] =
    ("-?\\d+".r <~ " +".r) ~ "-?\\d+".r ^^ { case x ~ y =>
      Pos(x.toInt, y.toInt)
    }

  def operator: Parser[Operator] =
    "==|!=|is|isnot".r ^^ { o =>
      if (o == "==" || o == "is") Operator.Equal else Operator.NotEqual
    }

  def matcher: Parser[Matcher] =
    fullMatcher | emptyMatcher | genericMatcher

  def fullMatcher: Parser[FullMatcher.type] =
    ("full" | "is +full".r | "isnot +empty".r) ^^ { _ => FullMatcher }

  def emptyMatcher: Parser[EmptyMatcher.type] =
    ("empty" | "is +empty".r | "isnot +full".r) ^^ { _ => EmptyMatcher }

  def genericMatcher: Parser[GenericMatcher] =
    (operator <~ ws) ~ rep1sep(tileMatcher, " " ~ ws) ^^ { case op ~ tms => GenericMatcher(op, tms*) }

  def tileMatcher: Parser[TileMatcher] =
    id ~ (dir | anyDir) ^^ { case i ~ d => TileMatcher(i, d) }

  def randomChance: Parser[Random] =
    "\\d+(?:\\.\\d*)?".r ~ "%?".r ^^ { case n ~ p =>
      if (p == "%") Random(n.toFloat) else Random(n.toFloat * 100)
    }

  def nocST: Parser[ShadowType] =
    ("noc" | "nooutsidecorner") ^^ { _ => ShadowType.NoOutsideCorner }

  def defST: Parser[ShadowType] =
    ("default" | "") ^^ { _ => ShadowType.Default }

  def sdST: Parser[ShadowType] =
    ("default" | "") ^^ { _ => ShadowType.SoftDigonals }

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
