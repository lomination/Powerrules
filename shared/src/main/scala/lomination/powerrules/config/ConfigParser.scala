package lomination.powerrules.config

import lomination.powerrules.util.{dropOnce, i}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object ConfigParser {

  val logger = org.log4s.getLogger

  lazy val property  = """([\w.]+) *= *([\S\s]+?) *""".r
  lazy val emptyLine = """\s*""".r

  def apply(configInput: String): Try[Config] =
    if (configInput.isEmpty)
      Success(Config())
    else
      process(Config(), configInput.split("\n").toSeq.zipWithIndex)

  @tailrec
  def process(config: Config, lines: Seq[(String, Int)]): Try[Config] =
    if (lines.isEmpty)
      Success(config)
    else
      lines.head match
        // empty line
        case (emptyLine(), l) =>
          logger.trace(s"Empty line found and skipped at line $l")
          process(config, lines.dropOnce)
        // parsing.indentation
        case (property(key, ToInt(int)), l) if key.toLowerCase == "parsing.indentation" =>
          logger.info(s"Config key `parsing.indentation` found at line $l successfully set to `$int`")
          process(config.setPIndentation(int), lines.dropOnce)
        case (property(key, value), l) if key.toLowerCase == "parsing.indentation" =>
          val exception = ConfigError(s"Value $value is not an integer at line $l")
          logger.error(exception)(s"Failed to parse the value of the key `parsing.indentation` at line $l (given `$value`)")
          Failure(exception)
        // parsing.useTabs
        case (property(key, ToBoolean(bool)), l) if key.toLowerCase == "parsing.usetabs" =>
          logger.info(s"Config key `parsing.usetabs` found at line $l successfully set to `$bool`")
          process(config.setPUseTabs(bool), lines.dropOnce)
        case (property(key, value), l) if key.toLowerCase == "parsing.usetabs" =>
          val exception = ConfigError(s"Value $value is not a boolean at line $l")
          logger.error(exception)(s"Failed to parse the value of the key `parsing.usetabs` at line $l (given `$value`)")
          Failure(exception)
        // writing.temporaryTile
        case (property(key, ToTmpTile(tmpTile)), l) if key.toLowerCase == "writing.temporarytile" =>
          logger.info(s"Config key `writing.temporaryTile` found at line $l successfully set to `$tmpTile`")
          process(config.setWTemporaryTile(tmpTile), lines.dropOnce)
        case (property(key, value), l) if key.toLowerCase == "writing.temporarytile" =>
          val exception = ConfigError(s"Value $value is not a temporary tile at line $l")
          logger.error(exception)(s"Failed to parse the value of the key `writing.temporaryTile` at line $l (given `$value`)")
          Failure(exception)
        // writing.spacing
        case (property(key, ToLevel(level)), l) if key.toLowerCase == "writing.spacing" =>
          logger.info(s"Config key `writing.spacing` found at line $l successfully set to `$level`")
          process(config.setWSpacing(level), lines.dropOnce)
        case (property(key, value), l) if key.toLowerCase == "writing.spacing" =>
          val exception = ConfigError(s"Value $value is not a level at line $l")
          logger.error(exception)(s"Failed to parse the value of the key `writing.spacing` at line $l (given `$value`)")
          Failure(exception)
        // writing.verbose
        case (property(key, ToLevel(level)), l) if key.toLowerCase == "writing.verbose" =>
          logger.info(s"Config key `writing.verbose` found at line $l successfully set to `$level`")
          process(config.setWVerbose(level), lines.dropOnce)
        case (property(key, value), l) if key.toLowerCase == "writing.verbose" =>
          val exception = ConfigError(s"Value $value is not a level at line $l")
          logger.error(exception)(s"Failed to parse the value of the key `writing.verbose` at line $l (given `$value`)")
          Failure(exception)
        // invalid property
        case (property(key, value), l) =>
          val exception = ConfigError(s"Invalid property name found `$key` at line $l")
          logger.error(exception)(s"Invalid property name found `$key` at line $l")
          Failure(exception)
        // regex failure
        case (line, l) =>
          val exception = ConfigError(s"No property found at line $l(`$line`)")
          logger.error(exception)("")
          Failure(exception)

  object ToInt:
    def unapply(string: String): Option[Int] =
      string.toIntOption.flatMap(int => if int > 0 then Some(int) else None)

  object ToBoolean:
    def unapply(string: String): Option[Boolean] =
      lazy val trueRe  = "[yY][eE][sS]|[tT][rR][uU][eE]|[uU][iI]".r
      lazy val falseRe = "[nN][oO]|[fF][aA][lL][sS][eE]".r
      string match
        case trueRe()  => Some(true)
        case falseRe() => Some(false)
        case _         => None

  object ToTmpTile:
    def unapply(string: String): Option[TmpTile] =
      val deci = "([0-9]+)".r
      val hexa = "0x([a-fA-F0-9]+)".r
      string match
        case deci(index) =>
          val i = Integer.parseInt(index)
          if 0 <= i && 0 < 256 then Some(TmpTile(i)) else None
        case hexa(index) =>
          val i = Integer.parseInt(index, 16)
          if 0 <= i && 0 < 256 then Some(TmpTile(i)) else None
        case _ => None

  object ToLevel:
    def unapply(string: String): Option[Level] =
      lazy val least  = "least".i
      lazy val less   = "less".i
      lazy val normal = "normal".i
      lazy val more   = "more".i
      lazy val most   = "most".i
      string match
        case least()  => Some(Level.Least)
        case less()   => Some(Level.Less)
        case normal() => Some(Level.Normal)
        case more()   => Some(Level.More)
        case most()   => Some(Level.Most)
        case _        => None

}
