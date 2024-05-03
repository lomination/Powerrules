package lomination.ddnettools.parser

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.*
import org.log4s.getLogger
import lomination.ddnettools.*

class RuleFileParser() extends RegexParsers {
  override protected val whiteSpace: Regex = "".r

  val logger = getLogger

  type P[X] = Parser[X]

  def apply(input: String): Try[RuleFile] =
    parseAll(rules, input) match
      case Success(result, next) =>
        scala.util.Success(result)
      case Failure(msg, next) =>
        val exception = IllegalArgumentException(s"Fail to parse RuleFile at ln ${next.pos.line}, col ${next.pos.column}.\n\n" + msg)
        scala.util.Failure(exception)
      case Error(msg, next) =>
        val exception = IllegalArgumentException(s"Fail to parse RuleFile at ln ${next.pos.line}, col ${next.pos.column} (fatal error).\n\n" + msg)
        scala.util.Failure(exception)

  // regex
  lazy val wsNl: Regex        = "[\n ]*\n".r
  lazy val sdTypeR: Regex     = "([+-])e([+-])i".r
  def ind(n: Int): Regex = List.fill(n)("  ").mkString.r

  // general
  lazy val rules: P[RuleFile]          = wsNl.? ~> (defaultTile <~ wsNl).? ~ rep1sep(rule, wsNl) <~ "[\n ]*".r ^^ { case d ~ r => RuleFile(d.getOrElse(DefaultTile(255, Dir.m3)), r) }
  lazy val defaultTile: P[DefaultTile] = ":" ~> numId ~ dir.? ^^ { case i ~ d => DefaultTile(i, d.getOrElse(Dir.p0)) }
  lazy val rule: P[Rule]               = (ruleName <~ wsNl) ~ repsep(command, wsNl) ^^ { case n ~ c => Rule(n, c) }
  lazy val ruleName: P[String]         = "\\[[^\n]+\\]".r ^^ { case s => s.drop(1).dropRight(1) }

  // commands
  lazy val command: P[Command] = replace | shadow | shape | comment
  // replace
  type ReReqStms = (WithStm)
  type ReOptStms = (Option[IfStm], Option[RandomStm], Option[RotateStm])
  lazy val replace: P[Replace] = ("replace" | "re") ~> reReqStms ~ reOptStms ^^ { case (withStm) ~ (ifStm, randomStm, rotateStm) =>
    Replace(
      withStm.tiles,
      ifStm.getOrElse(IfStm(Seq())).conds,
      randomStm.getOrElse(RandomStm(Random.always)).chance,
      rotateStm.getOrElse(RotateStm(Seq(Dir.p0))).rotations
    )
  }
  lazy val reReqStms: P[ReReqStms] = withStm
  lazy val reOptStms: P[ReOptStms] = (ifStm | randStm | rotStm).* ^^ { seq =>
    (
      seq.collectFirst { case stm: IfStm => stm },
      seq.collectFirst { case stm: RandomStm => stm },
      seq.collectFirst { case stm: RotateStm => stm }
    )
  }
  // shadow
  type SdReqStm = (WithStm)
  type SdOptStm = (Option[IfStm], Option[TypeStm])
  lazy val shadow: P[Shadow] = ("shadow" | "sd") ~> sdReqStm ~ sdOptStm ^^ { case (withStm) ~ (ifStm, typeStm) =>
    Shadow(
      withStm.tiles,
      ifStm.getOrElse(IfStm(Seq())).conds,
      typeStm.getOrElse(TypeStm(ShadowType.default)).sdType
    )
  }
  lazy val sdReqStm: P[SdReqStm] = withStm
  lazy val sdOptStm: P[SdOptStm] = (ifStm | typeStm).* ^^ { seq =>
    (
      seq.collectFirst { case stm: IfStm => stm },
      seq.collectFirst { case stm: TypeStm => stm }
    )
  }
  // shape
  type SpReqStm = (ApplyStm, OnStm, UsingStm, NeutralStm)
  type SpOptStm = (Option[RandomStm], Option[RotateStm])
  lazy val shape: P[Shape] = ("shape" | "sp") ~> spReqStm ~ spOptStm ^^ { case (applyStm, onStm, usingStm, neutralStm) ~ (randomStm, rotateStm) =>
    val newMap = (usingStm.map ++ Map('!' -> EmptyMatcher, '.' -> FullMatcher)).toMap
    Shape(
      applyStm.chars.map(c =>
        newMap.get(c) match
          case Some(t: Tile)    => Some(t)
          case Some(m: Matcher) => None
          case None             => None
      ),
      onStm.chars.map(c =>
        newMap.get(c) match
          case Some(t: Tile)    => Some(GenericMatcher(Op.Is, t.toTileMatcher))
          case Some(m: Matcher) => Some(m)
          case None             => None
      ),
      neutralStm.tile,
      randomStm.getOrElse(RandomStm(Random.always)).chance,
      rotateStm.getOrElse(RotateStm(Seq(Dir.p0))).rotations
    )
  }
  lazy val spReqStm: P[SpReqStm] = (applyStm | onStm | usingStm | neutralStm).+ ^^ { seq =>
    (
      seq.collectFirst { case stm: ApplyStm => stm }.get,
      seq.collectFirst { case stm: OnStm => stm }.get,
      seq.collectFirst { case stm: UsingStm => stm }.get,
      seq.collectFirst { case stm: NeutralStm => stm }.get
    )
  }
  lazy val spOptStm: P[SpOptStm] = (randStm | rotStm).* ^^ { seq =>
    (
      seq.collectFirst { case stm: RandomStm => stm },
      seq.collectFirst { case stm: RotateStm => stm }
    )
  }
  // comment
  lazy val comment: P[Comment] = "#[^\n]*".r ^^ { case str => Comment(str) }

  // statements
  def stm[A](name: P[String])(content: P[A]) = (" +".r | (wsNl ~ ind(1))) ~ name ~ (" +".r | (wsNl ~ ind(2))) ~> content
  lazy val withStm: P[WithStm]               = stm("with")(rep1sep(tile, " +".r | (wsNl ~ ind(2)))) ^^ { WithStm(_) }
  lazy val ifStm: P[IfStm]                   = stm("if" | "when")(rep1sep(cond, " +& +".r | (" +&".r ~ wsNl ~ ind(2)))) ^^ { IfStm(_) }
  lazy val randStm: P[RandomStm]             = stm("random")(random) ^^ { RandomStm(_) }
  lazy val rotStm: P[RotateStm]              = stm("rotate")(rep1sep(dir, " *".r | (wsNl ~ ind(2)))) ^^ { RotateStm(_) }
  lazy val typeStm: P[TypeStm]               = stm("type")(sdType) ^^ { TypeStm(_) }
  lazy val applyStm: P[ApplyStm]             = stm("apply")(charGrid) ^^ { ApplyStm(_) }
  lazy val onStm: P[OnStm]                   = stm("on")(charGrid) ^^ { OnStm(_) }
  lazy val usingStm: P[UsingStm]             = stm("using")(rep1sep(mapLine, wsNl ~ ind(2))) ^^ { (m: Seq[(Char, Tile | Matcher)]) => UsingStm(m.toMap) }
  lazy val neutralStm: P[NeutralStm]         = stm("neutral")(tile) ^^ { NeutralStm(_) }

  // others
  lazy val cond: P[Cond]                      = (pos <~ " +".r) ~ matcher ^^ { case p ~ m => Cond(p, m) }
  lazy val pos: P[Pos]                        = ("-?\\d+".r <~ " +".r) ~ "-?\\d+".r ^^ { case x ~ y => Pos(x.toInt, y.toInt) }
  lazy val matcher: P[Matcher]                = fullM | emptyM | genericM
  lazy val fullM: P[FullMatcher.type]         = ("full" | "is +full".r | "isnot +empty".r) ^^ { _ => FullMatcher }
  lazy val emptyM: P[EmptyMatcher.type]       = ("empty" | "is +empty".r | "isnot +full".r) ^^ { _ => EmptyMatcher }
  lazy val genericM: P[GenericMatcher]        = (op <~ " +".r) ~ rep1sep(tileM, " *\\| *".r) ^^ { case op ~ tms => GenericMatcher(op, tms*) }
  lazy val op: P[Op]                          = "isnot|is".r ^^ { o => if (o == "is") Op.Is else Op.Isnot }
  lazy val tileM: P[TileMatcher]              = (numId | outsideId) ~ (dir | anyDir).? ^^ { case id ~ dir => TileMatcher(id, dir.getOrElse(AnyDir)) }
  lazy val anyDir: P[AnyDir.type]             = "*" ^^ { _ => AnyDir }
  lazy val tile: P[Tile]                      = (numId | outsideId) ~ dir.? ^^ { case i ~ d => Tile(i, d.getOrElse(Dir.p0)) }
  lazy val numId: P[Int]                      = "[a-f0-9]{1,2}".r ^^ { Integer.parseInt(_, 16) }
  lazy val outsideId: P[Int]                  = "outside" ^^ { _ => -1 }
  lazy val dir: P[Dir]                        = "[+-]".r ~ "[0-3]".r ^^ { case s ~ t => Dir(if (s == "+") Sign.+ else Sign.-, Times.fromOrdinal(t.toInt)) }
  lazy val random: P[Random]                  = "\\d+(?:\\.\\d*)?".r ~ "%?".r ^^ { case n ~ p => if (p == "%") Random(n.toFloat) else Random(n.toFloat * 100) }
  lazy val charGrid: P[Grid[Char]]            = rep1sep(rep1sep(char, " *".r), wsNl ~ ind(2)) ^^ { Grid(_) }
  lazy val char: P[Char]                      = "[\\S]".r ^^ { _.charAt(0) }
  lazy val mapLine: P[(Char, Tile | Matcher)] = ("[\\S]".r <~ " +-> +".r) ~ (tile | matcher) ^^ { case c ~ mt => c.charAt(0) -> mt }
  lazy val sdType: P[ShadowType]              = "[+-]e[+-]i".r ^^ { case sdTypeR(e, i) => ShadowType(if (e == "+") true else false, if (i == "+") true else false) }
}
