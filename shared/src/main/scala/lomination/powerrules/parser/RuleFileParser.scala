package lomination.powerrules.parser

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.*
import lomination.powerrules.*

class RuleFileParser() extends RegexParsers {
  override protected val whiteSpace: Regex = "".r

  val logger = org.log4s.getLogger

  type P[X] = Parser[X]

  def apply(input: String): Try[RuleFile] =
    parseAll(rules, input) match
      case Success(result, next) =>
        logger.info("Parser succeed")
        scala.util.Success(result)
      case Error(msg, next) =>
        val exception = ParsingError(s"Handled error: fail to parse rule file (at l:${next.pos.line}, c:${next.pos.column}).\n\n" + msg)
        logger.warn(exception)("Parser generated a handled error (parse result = error)")
        scala.util.Failure(exception)
      case Failure(msg, next) =>
        val exception = ParsingError(s"Not handled failure: fail to parse rule file (at l:${next.pos.line}, c:${next.pos.column}).\n\n" + msg)
        logger.warn(exception)("Parser generated an unhandled error (parse result = failure)")
        scala.util.Failure(exception)

  def pureParser[T](value: T): Parser[T] = new Parser[T] { override def apply(in: Input): ParseResult[T] = Success(value, in)  }

  // regex
  lazy val wsNl: Regex   = "[\n ]*\n".r
  def ind(n: Int): Regex = List.fill(n)("  ").mkString.r

  // general
  lazy val rules: P[RuleFile]  = wsNl.? ~> (tmpTile <~ wsNl).? ~ rep1sep(rule, wsNl) <~ "[\n ]*".r ^^ { case d ~ r => RuleFile(d.getOrElse(TmpTile(255, Dir.m3)), r) }
  lazy val tmpTile: P[TmpTile] = ":" ~> (numericId | errNumericId) ~! (dir | errDir).? ^^ { case i ~ d => TmpTile(i, d.getOrElse(Dir.p0)) }
  lazy val rule: P[Rule]       = ((ruleName <~ wsNl) | errRuleName) ~! repsep(command, wsNl) ^^ { case n ~ c => Rule(n, c) }
  lazy val ruleName: P[String] = "\\[[^\n]+\\]".r ^^ { case s => s.drop(1).dropRight(1) }

  // commands
  lazy val command: P[Command] = replace | shadow | shape | comment | errCommand
  // replace
  type ReReqStms = (WithStm)                                             // replace required statements
  type ReOptStms = (Option[IfStm], Option[RandomStm], Option[RotateStm]) // replace optional statements
  lazy val replace: P[Replace] = ("replace" | "re") ~>! reReqStms ~ reOptStms ^^ { case (withStm) ~ (ifStm, randomStm, rotateStm) =>
    Replace(
      withStm.tiles,
      ifStm.getOrElse(IfStm(Seq())).conds,
      randomStm.getOrElse(RandomStm(Random.always)).chance,
      rotateStm.getOrElse(RotateStm(Seq(Dir.p0))).rotations
    )
  }
  lazy val reReqStms: P[ReReqStms] = (withStm | errReReqStms) // replace required statements
  lazy val reOptStms: P[ReOptStms] = (ifStm | randStm | rotStm | errReOptStms).* ^^ { seq => // replace optional statements
    (
      seq.collectFirst { case stm: IfStm => stm },
      seq.collectFirst { case stm: RandomStm => stm },
      seq.collectFirst { case stm: RotateStm => stm }
    )
  }
  // shadow
  type SdReqStm = (WithStm)                                                                          // shadow required statements
  type SdOptStm = (Option[WithExternalStm], Option[WithInternalStm], Option[IfStm], Option[ModeStm]) // shadow optional statements
  lazy val shadow: P[Shadow] = ("shadow" | "sd") ~>! sdReqStm ~ sdOptStm ^^ { case (withStm) ~ (withExtStm, withIntStm, ifStm, modeStm) =>
    Shadow(
      withStm.tiles,
      withExtStm.getOrElse(WithExternalStm(Seq())).tiles,
      withIntStm.getOrElse(WithInternalStm(Seq())).tiles,
      ifStm.getOrElse(IfStm(Seq())).conds,
      modeStm.getOrElse(ModeStm(false)).softMode
    )
  }
  lazy val sdReqStm: P[SdReqStm] = (withStm | errSdReqStms) // shadow required statements
  lazy val sdOptStm: P[SdOptStm] = (withExtStm | withIntStm | ifStm | modeStm | errSdOptStms).* ^^ { seq => // shadow optional statements
    (
      seq.collectFirst { case stm: WithExternalStm => stm },
      seq.collectFirst { case stm: WithInternalStm => stm },
      seq.collectFirst { case stm: IfStm => stm },
      seq.collectFirst { case stm: ModeStm => stm }
    )
  }
  // shape
  type SpReqStm = (ApplyStm, OnStm, UsingStm, NeutralStm, RandomStm) // shape required statements
  type SpOptStm = (Option[RotateStm])
  lazy val shape: P[Shape] = ("shape" | "sp") ~>! spReqStm ~ spOptStm ^^ { case (applyStm, onStm, usingStm, neutralStm, randomStm) ~ (rotateStm) =>
    val newMap = usingStm.map ++ Map('!' -> FullMatcher(Op.Isnot), '.' -> FullMatcher(Op.Is)) - '?'
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
      randomStm.chance,
      rotateStm.getOrElse(RotateStm(Seq(Dir.p0))).rotations
    )
  }
  lazy val spReqStm: P[SpReqStm] = (applyStm | onStm | usingStm | neutralStm | randStm | errSpReqStms).+ into { seq => // shape required statements
    for {
      applyStm   <- seq.collectFirst { case stm: ApplyStm => stm }.fold[Parser[ApplyStm]](err("Missing apply statement in shape command"))(pureParser)
      onStm      <- seq.collectFirst { case stm: OnStm => stm }.fold[Parser[OnStm]](err("Missing on statement in shape command"))(pureParser)
      usingStm   <- seq.collectFirst { case stm: UsingStm => stm }.fold[Parser[UsingStm]](err("Missing using statement in shape command"))(pureParser)
      neutralStm <- seq.collectFirst { case stm: NeutralStm => stm }.fold[Parser[NeutralStm]](err("Missing neutral statement in shape command"))(pureParser)
      randomStm  <- seq.collectFirst { case stm: RandomStm => stm }.fold[Parser[RandomStm]](err("Missing random statement in shape command"))(pureParser)
    } yield (applyStm, onStm, usingStm, neutralStm, randomStm)
  }
  lazy val spOptStm: P[SpOptStm] = (rotStm.? | errSpReqStms.?) // shape optional statements
  // comment
  lazy val comment: P[Comment] = "#[^\n]*".r ^^ { case str => Comment(str) }

  // statements
  def stm[A](name: P[String])(content: P[A]) = (" +".r | (wsNl ~ ind(1))) ~ name ~ (" +".r | (wsNl ~ ind(2))) ~> content
  lazy val withStm: P[WithStm]               = stm("with")(rep1sep(tile, " +".r | (wsNl ~ ind(2)))) ^^ { WithStm(_) }
  lazy val withExtStm: P[WithExternalStm]    = stm("withext" | "withe")(rep1sep(tile, " +".r | (wsNl ~ ind(2)))) ^^ { WithExternalStm(_) }
  lazy val withIntStm: P[WithInternalStm]    = stm("withint" | "withi")(rep1sep(tile, " +".r | (wsNl ~ ind(2)))) ^^ { WithInternalStm(_) }
  lazy val ifStm: P[IfStm]                   = stm("if" | "when")(rep1sep(cond, " +& +".r | (" +&".r ~ wsNl ~ ind(2)))) ^^ { IfStm(_) }
  lazy val randStm: P[RandomStm]             = stm("random")(random) ^^ { RandomStm(_) }
  lazy val rotStm: P[RotateStm]              = stm("rotate")(rep1sep(dir, " *".r | (wsNl ~ ind(2)))) ^^ { RotateStm(_) }
  lazy val modeStm: P[ModeStm]               = stm("mode")(mode) ^^ { ModeStm(_) }
  lazy val applyStm: P[ApplyStm]             = stm("apply")(charGrid) ^^ { ApplyStm(_) }
  lazy val onStm: P[OnStm]                   = stm("on")(charGrid) ^^ { OnStm(_) }
  lazy val usingStm: P[UsingStm]             = stm("using")(rep1sep(mapLine, wsNl ~ ind(2))) ^^ { (m: Seq[(Char, Tile | GenericMatcher)]) => UsingStm(m.toMap) }
  lazy val neutralStm: P[NeutralStm]         = stm("neutral")(tile) ^^ { NeutralStm(_) }

  // others
  lazy val cond: P[Cond]                             = (pos | errPos <~ " +".r) ~ matcher ^^ { case p ~ m => Cond(p, m) }
  lazy val pos: P[Pos]                               = ("-?\\d+".r <~ " +".r) ~ "-?\\d+".r ^^ { case x ~ y => Pos(x.toInt, y.toInt) }
  lazy val matcher: P[Matcher]                       = fullM | notEdgeM | genericM | errEdgeM | errMatcher
  lazy val fullM: P[FullMatcher]                     = (op | errOp <~ " +".r) ~ ("full" | "empty") ^^ { case o ~ w => FullMatcher(if (w == "full") o else o.not) }
  lazy val notEdgeM: P[NotEdgeMatcher.type]          = "isnot +edge".r ^^ { _ => NotEdgeMatcher }
  lazy val genericM: P[GenericMatcher]               = (op | errOp <~ " +".r) ~ rep1sep(tileM, " *\\| *".r) ^^ { case op ~ tms => GenericMatcher(op, tms*) }
  lazy val op: P[Op]                                 = "isnot|is".r ^^ { o => if (o == "is") Op.Is else Op.Isnot }
  lazy val tileM: P[TileMatcher]                     = (numericId | outsideId | errId) ~ (dir | anyDir | errDirOrAnyDir).? ^^ { case id ~ dir => TileMatcher(id, dir.getOrElse(AnyDir)) }
  lazy val anyDir: P[AnyDir.type]                    = "*" ^^ { _ => AnyDir }
  lazy val tile: P[Tile]                             = (numericId | errNumericId) ~ (dir.? | errDir.?) ^^ { case i ~ d => Tile(i, d.getOrElse(Dir.p0)) }
  lazy val numericId: P[Int]                         = "[a-f0-9]{1,2}".r ^^ { Integer.parseInt(_, 16) }
  lazy val outsideId: P[Int]                         = ("outside" | "-1") ^^ { _ => -1 }
  lazy val dir: P[Dir]                               = "[+-]".r ~ "[0-3]".r ^^ { case s ~ t => Dir(if (s == "+") Sign.+ else Sign.-, Times.fromOrdinal(t.toInt)) }
  lazy val random: P[Random]                         = "\\d+(?:\\.\\d+)?".r ~ "%?".r ^^ { case n ~ p => if (p == "%") Random(n.toFloat) else Random(n.toFloat * 100) }
  lazy val charGrid: P[Grid[Char]]                   = rep1sep(rep1sep(char, " *".r), wsNl ~ ind(2)) ^^ { Grid(_) }
  lazy val char: P[Char]                             = "[\\S]".r ^^ { _.charAt(0) }
  lazy val mapLine: P[(Char, Tile | GenericMatcher)] = ("[\\S]".r <~ " *-> *".r) ~ (tile | genericM) ^^ { case c ~ tOrTm => c.charAt(0) -> tOrTm }
  lazy val mode: P[Boolean]                          = "soft|normal".r ^^ { case m => m == "soft" }

  // errors
  def wiki(pages: String*): String = pages.map((p: String) => s"https://github.com/lomination/Powerrules/wiki/${p}").mkString("\n\nSee wiki:\n", ",\n", "") // github wiki reference
  lazy val errRuleName             = not("\\[[^\n]+\\]") >> (_ => err("No rule title found." + wiki("Rule")))
  lazy val errCommand              = "\\S+".r - ("replace" | "re" | "shadow" | "sd" | "shape" | "sp") >> (_ => err("Command not found. Did you mean 'replace', 're', shadow', 'sd', 'shape', 'sp' or a comment?" + wiki("Command")))
  lazy val errReReqStms            = (" +".r | (wsNl ~ ind(1))) ~> not("with") >> (_ => err("The given statement is not valid. Expected statements: with statement." + wiki("Replace", "Command")))
  lazy val errReOptStms            = (" +".r | (wsNl ~ ind(1))) ~> not("if" | "when" | "random" | "rotate") >> (_ => err("The given statement is not valid. Expected statements: if, random or rotate statments." + wiki("Replace", "Command")))
  lazy val errSdReqStms            = (" +".r | (wsNl ~ ind(1))) ~> not("with") >> (_ => err("The given statement is not valid. Expected statements: with statement." + wiki("Shadow", "Command")))
  lazy val errSdOptStms            = (" +".r | (wsNl ~ ind(1))) ~> not("withint" | "withi" | "withext" | "withe" | "if" | "mode") >> (_ => err("The given statement is not valid. Expected statements: withext, withint, if or mode statements." + wiki("Shadow", "Command")))
  lazy val errSpReqStms            = (" +".r | (wsNl ~ ind(1))) ~> not("apply" | "on" | "using" | "neutral") >> (_ => err("The given statement is not valid. Expected statements: apply, on, using, or neutral statements." + wiki("Shape", "Command")))
  lazy val errSpOptStms            = (" +".r | (wsNl ~ ind(1))) ~> not("rotate") >> (_ => err("The given statement is not valid. Expected statements: rotate statement." + wiki("Shape", "Command")))
  lazy val errPos                  = err("The given postition is not valid. Use 'x y' where x and y are signed intergers." + wiki("Condition#Position"))
  lazy val errEdgeM                = "is +edge".r >> (_ => err("The edge matcher cannot be positive due to language restriction. Please do not use 'is edge'" + wiki("Condition#edge-matcher")))
  lazy val errMatcher              = err("The given matcher is not valid." + wiki("Condition#matcher"))
  lazy val errOp                   = err("The given operator is not valid. Expected 'is' or 'isnot'." + wiki("Condition#Operator"))
  lazy val errId                   = err("The given tile matcher id is not valid. Expected either a hexadecimal value between '0' and 'ff', or '-1', or 'outside' keyword." + wiki("Condition#generic-matcher"))
  lazy val errDirOrAnyDir          = err("The given tile matcher direction is not valid. Expected a direction or an any direction wildcard '*'." + wiki("Tile#any-direction"))
  lazy val errNumericId            = err("numid err")
  lazy val errDir                  = err("err dir")
}
