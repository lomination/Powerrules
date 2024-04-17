package lomination.ddnettools.parser

import scala.util.parsing.combinator.*
import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import scala.util.Try
import org.log4s.getLogger
import lomination.ddnettools.*

class RuleFileParser() extends RegexParsers {
  override protected val whiteSpace: Regex = "".r

  val logger = getLogger

  def apply(input: String): Try[RuleFile] =
    parseAll(rules, input) match
      case Success(result, next) =>
        scala.util.Success(result)
      case Failure(msg, next)    =>
        val exception = IllegalArgumentException(s"Fail to parse RuleFile at ln ${next.pos.line}, col ${next.pos.column} (fatal error)" + msg)
        // logger.error(exception)(s"Fail to parse RuleFile at ln ${next.pos.line}, col ${next.pos.column}")
        scala.util.Failure(exception)
      case Error(msg, next)      =>
        val exception = IllegalArgumentException(s"Fail to parse RuleFile at ln ${next.pos.line}, col ${next.pos.column} (fatal error)" + msg)
        // logger.error(exception)(s"Fail to parse RuleFile at ln ${next.pos.line}, col ${next.pos.column} (fatal error)")
        scala.util.Failure(exception)

  // regex
  val sp: Regex   = " *".r       // space
  val ws: Regex   = "[\n ]*".r   // any white space character (new line or space)
  val wsNl: Regex = "[\n ]*\n".r // any white space followed by a new line
  val sdTypeR: Regex = "([+-])e([+-])i([+-])s".r

  def ind(n: Int): Regex = List.fill(n)("  ").mkString.r // indent

  // general
  def rules: Parser[RuleFile]          = ws ~> (defaultTile <~ wsNl).? ~ rep1sep(rule, wsNl) <~ ws ^^ { case d ~ r => RuleFile(d.getOrElse(DefaultTile(255, Dir.m3)), r) }
  def defaultTile: Parser[DefaultTile] = ":" ~> id ~ dir.? ^^ { case i ~ d => DefaultTile(i, d.getOrElse(Dir.m3)) }
  def rule: Parser[Rule]               = (ruleName <~ wsNl) ~ repsep(command, wsNl) ^^ { case n ~ c => Rule(n, c) }
  def ruleName: Parser[String]         = "\\[[^\n]+\\]".r ^^ { case s => s.drop(1).dropRight(1) }

  // commands
  def command: Parser[Command] = replace | shadow | comment // | shape
  // replace
  type PreReplace = Seq[Tile] ~ Option[Seq[Cond]] ~ Option[Random] ~ Option[Seq[Dir]]
  def replace: Parser[Replace] = re ^^ { case t ~ c ~ r ~ a => Replace(t, c.getOrElse(Seq()), r.getOrElse(Random.always), a.getOrElse(Seq(Dir.default))) }
  def re: Parser[PreReplace]   = reKw ~> wS ~ iS.? ~ raS.? ~ roS.?
  def reKw: Parser[Unit]       = ("replace" | "re") ^^ { _ => () }
  // shadow
  type PreShadow = Seq[Tile] ~ Option[Seq[Cond]] ~ Option[ShadowType]
  def shadow: Parser[Shadow] = sd ^^ { case t ~ c ~ st => Shadow(t, c.getOrElse(Seq(Pos(0, 0) is FullMatcher)), st.getOrElse(ShadowType.default)) }
  def sd: Parser[PreShadow]  = sdKw ~> wS ~ iS.? ~ tS.?
  def sdKw: Parser[Unit]     = ("shadow" | "sd") ^^ { _ => () }
  // comment
  def comment: Parser[Comment] = "#[^\n]*".r ^^ { case str => Comment(str) }

  // statements
  def stm[A](name: Parser[String], content: Parser[A]) = (" +".r | (wsNl ~ ind(1))) ~ name ~ (" +".r | (wsNl ~ ind(2))) ~> content

  def wS: Parser[Seq[Tile]]  = stm("with", rep1sep(tile, " +".r | (wsNl ~ ind(2))))
  def iS: Parser[Seq[Cond]]  = stm(("if" | "when"), rep1sep(cond, " +& +".r | (" +&".r ~ wsNl ~ ind(2))))
  def raS: Parser[Random]    = stm("random", random)
  def roS: Parser[Seq[Dir]]  = stm("rotate", rep1sep(dir, sp | (wsNl ~ ind(2))))
  def tS: Parser[ShadowType] = stm("type", sdType)

  // others
  // cond
  def cond: Parser[Cond]   = (pos <~ sp) ~ matcher ^^ { case p ~ m => Cond(p, m) }
  def pos: Parser[Pos]     = ("-?\\d+".r <~ " +".r) ~ "-?\\d+".r ^^ { case x ~ y => Pos(x.toInt, y.toInt) }
  // matcher
  def matcher: Parser[Matcher]          = fullM | emptyM | genericM
  def fullM: Parser[FullMatcher.type]   = ("full" | "is +full".r | "isnot +empty".r) ^^ { _ => FullMatcher }
  def emptyM: Parser[EmptyMatcher.type] = ("empty" | "is +empty".r | "isnot +full".r) ^^ { _ => EmptyMatcher }
  def genericM: Parser[GenericMatcher]  = (op <~ " +".r) ~ rep1sep(tileM, " *\\| *".r) ^^ { case op ~ tms => GenericMatcher(op, tms*) }
  def op: Parser[Operator]              = "isnot|is".r ^^ { o => if (o == "is") Operator.Equal else Operator.NotEqual }
  def tileM: Parser[TileMatcher]        = id ~ (dir | anyDir) ^^ { case i ~ d => TileMatcher(i, d) }
  def anyDir: Parser[AnyDir.type]       = "*" ^^ { _ => AnyDir }
  // tile
  def tile: Parser[Tile] = id ~ dir.? ^^ { case i ~ d => Tile(i, d.getOrElse(Dir.default)) }
  def id: Parser[Int]    = "[a-f0-9]{1,2}".r ^^ { i => Integer.parseInt(i, 16) }
  def dir: Parser[Dir]   = "[+-]".r ~ "[0-3]".r ^^ { case s ~ t => Dir(if (s == "+") Sign.+ else Sign.-, Times.fromOrdinal(t.toInt)) }
  // others
  def random: Parser[Random]         = "\\d+(?:\\.\\d*)?".r ~ "%?".r ^^ { case n ~ p => if (p == "%") Random(n.toFloat) else Random(n.toFloat * 100) }
  def sdType: Parser[ShadowType]     = "[+-]e[+-]i[+-]s".r ^^ { case sdTypeR(e, i, s) =>
    def bool(sign: String) = if (sign == "+") true else false
    ShadowType(bool(e), bool(i), bool(s))
  }
}
