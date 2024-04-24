package lomination.ddnettools.parser

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.*
import org.log4s.getLogger
import lomination.ddnettools.*

class RuleFileParser() extends RegexParsers {
  override protected val whiteSpace: Regex = "".r

  val logger = getLogger

  def apply(input: String): Try[RuleFile] =
    parseAll(rules, input) match
      case Success(result, next) =>
        scala.util.Success(result)
      case Failure(msg, next) =>
        val exception = IllegalArgumentException(s"Fail to parse RuleFile at ln ${next.pos.line}, col ${next.pos.column}.\n\n" + msg)
        // logger.error(exception)(s"Fail to parse RuleFile at ln ${next.pos.line}, col ${next.pos.column}")
        scala.util.Failure(exception)
      case Error(msg, next) =>
        val exception = IllegalArgumentException(s"Fail to parse RuleFile at ln ${next.pos.line}, col ${next.pos.column} (fatal error).\n\n" + msg)
        // logger.error(exception)(s"Fail to parse RuleFile at ln ${next.pos.line}, col ${next.pos.column} (fatal error)")
        scala.util.Failure(exception)

  // regex
  val spa: Regex     = " *".r       // space
  val ws: Regex      = "[\n ]*".r   // any white space character (new line or space)
  val wsNl: Regex    = "[\n ]*\n".r // any white space followed by a new line
  val sdTypeR: Regex = "([+-])e([+-])i".r

  def ind(n: Int): Regex = List.fill(n)("  ").mkString.r // indent

  def newShape() = ???
  // val newMap = map ++ Map('!' -> EmptyMatcher, '.' -> FullMatcher)
  //   Shape(
  //     newP.map(
  //       _.map(c =>
  //         newMap.get(c) match
  //           case Some(t: Tile)    => t
  //           case Some(m: Matcher) => throw IllegalArgumentException("")
  //           case None             => throw IllegalArgumentException("")
  //       )
  //     ),
  //     (for {
  //       x <- 0 until oldP(0).length
  //       y <- 0 until oldP.length
  //       c = oldP(y)(x)
  //       if c == '?'
  //     } yield newMap.get(c) match
  //       case Some(m: Matcher) => (Pos(x, y) is m)
  //       case Some(t: Tile)    => (Pos(x, y) is t.toTileMatcher)
  //       case None             => throw IllegalArgumentException("")
  //     ),
  //     ra.getOrElse(Random.always),
  //     ro.getOrElse(Seq(Dir.p0))
  //   )

  // general
  def rules: Parser[RuleFile]          = ws ~> (defaultTile <~ wsNl).? ~ rep1sep(rule, wsNl) <~ ws ^^ { case d ~ r => RuleFile(d.getOrElse(DefaultTile(255, Dir.m3)), r) }
  def defaultTile: Parser[DefaultTile] = ":" ~> id ~ dir.? ^^ { case i ~ d => DefaultTile(i, d.getOrElse(Dir.m3)) }
  def rule: Parser[Rule]               = (ruleName <~ wsNl) ~ repsep(command, wsNl) ^^ { case n ~ c => Rule(n, c) }
  def ruleName: Parser[String]         = "\\[[^\n]+\\]".r ^^ { case s => s.drop(1).dropRight(1) }

  // commands
  def command: Parser[Command] = replace | shadow | shape | comment
  // replace
  type PreReplace = Seq[Tile] ~ Option[Seq[Cond]] ~ Option[Random] ~ Option[Seq[Dir]]
  def replace: Parser[Replace] = re ^^ { case t ~ c ~ ra ~ ro => Replace(t, c.getOrElse(Seq()), ra.getOrElse(Random.always), ro.getOrElse(Seq(Dir.p0))) }
  def re: Parser[PreReplace]   = reKw ~> whS ~ ifS.? ~ raS.? ~ roS.?
  def reKw: Parser[Unit]       = ("replace" | "re") ^^ { _ => () }
  // shadow
  type PreShadow = Seq[Tile] ~ Option[Seq[Cond]] ~ Option[ShadowType]
  def shadow: Parser[Shadow] = sd ^^ { case t ~ c ~ st => Shadow(t, c.getOrElse(Seq()), st.getOrElse(ShadowType.default)) }
  def sd: Parser[PreShadow]  = sdKw ~> whS ~ ifS.? ~ tyS.?
  def sdKw: Parser[Unit]     = ("shadow" | "sd") ^^ { _ => () }
  // shape
  type PreShape = Seq[Seq[Char]] ~ Seq[Seq[Char]] ~ Map[Char, Tile | Matcher] ~ Option[Random] ~ Option[Seq[Dir]]
  def shape: Parser[Shape] = sp ^^ { case newP ~ oldP ~ map ~ ra ~ ro => newShape() }
  def sp: Parser[PreShape] = spKw ~> paS ~ paS ~ usS ~ raS.? ~ roS.?
  def spKw: Parser[Unit]   = ("shape" | "sp") ^^ { _ => () }

  // comment
  def comment: Parser[Comment] = "#[^\n]*".r ^^ { case str => Comment(str) }

  // statements
  def stm[A](name: Parser[String], content: Parser[A]) = (" +".r | (wsNl ~ ind(1))) ~ name ~ (" +".r | (wsNl ~ ind(2))) ~> content

  def whS: Parser[Seq[Tile]]                 = stm("with", rep1sep(tile, " +".r | (wsNl ~ ind(2))))
  def ifS: Parser[Seq[Cond]]                 = stm("if" | "when", rep1sep(cond, " +& +".r | (" +&".r ~ wsNl ~ ind(2))))
  def raS: Parser[Random]                    = stm("random", random)
  def roS: Parser[Seq[Dir]]                  = stm("rotate", rep1sep(dir, spa | (wsNl ~ ind(2))))
  def tyS: Parser[ShadowType]                = stm("type", sdType)
  def paS: Parser[Seq[Seq[Char]]]            = stm("pattern", charPattern)
  def usS: Parser[Map[Char, Tile | Matcher]] = stm("using", rep1sep(mapLine, wsNl ~ ind(2))) ^^ { _.toMap }

  // others
  // cond
  def cond: Parser[Cond] = (pos <~ spa) ~ matcher ^^ { case p ~ m => Cond(p, m) }
  def pos: Parser[Pos]   = ("-?\\d+".r <~ " +".r) ~ "-?\\d+".r ^^ { case x ~ y => Pos(x.toInt, y.toInt) }
  // matcher
  def matcher: Parser[Matcher]          = fullM | emptyM | genericM
  def fullM: Parser[FullMatcher.type]   = ("full" | "is +full".r | "isnot +empty".r) ^^ { _ => FullMatcher }
  def emptyM: Parser[EmptyMatcher.type] = ("empty" | "is +empty".r | "isnot +full".r) ^^ { _ => EmptyMatcher }
  def genericM: Parser[GenericMatcher]  = (op <~ " +".r) ~ rep1sep(tileM, " *\\| *".r) ^^ { case op ~ tms => GenericMatcher(op, tms*) }
  def op: Parser[Operator]              = "isnot|is".r ^^ { o => if (o == "is") Operator.Equal else Operator.NotEqual }
  def tileM: Parser[TileMatcher]        = (id | "outside") ~ (dir | anyDir).? ^^ { case id ~ dir => TileMatcher(id match { case id: Int => id; case _: String => -1 }, dir.getOrElse(AnyDir)) }
  def anyDir: Parser[AnyDir.type]       = "*" ^^ { _ => AnyDir }
  // tile
  def tile: Parser[Tile] = id ~ dir.? ^^ { case i ~ d => Tile(i, d.getOrElse(Dir.p0)) }
  def id: Parser[Int]    = "[a-f0-9]{1,2}".r ^^ { i => Integer.parseInt(i, 16) }
  def dir: Parser[Dir]   = "[+-]".r ~ "[0-3]".r ^^ { case s ~ t => Dir(if (s == "+") Sign.+ else Sign.-, Times.fromOrdinal(t.toInt)) }
  // random
  def random: Parser[Random] = "\\d+(?:\\.\\d*)?".r ~ "%?".r ^^ { case n ~ p => if (p == "%") Random(n.toFloat) else Random(n.toFloat * 100) }
  // shadow
  def sdType: Parser[ShadowType] = "[+-]e[+-]i".r ^^ { case sdTypeR(e, i) =>
    def bool(sign: String) = if (sign == "+") true else false
    ShadowType(bool(e), bool(i))
  }
  // shape
  def mapLine: Parser[(Char, Tile | Matcher)] = ("[^ \n]".r <~ " +-> +".r) ~ (tile | matcher) ^^ { case c ~ mt => c.charAt(0) -> mt }
  def charPattern: Parser[Seq[Seq[Char]]]     = rep1sep(rep1sep(char, " *".r), wsNl ~ ind(2))
  def char: Parser[Char]                      = "[\\S]".r ^^ { _.charAt(0) }
}
