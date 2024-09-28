package lomination.powerrules

import lomination.powerrules.config.ConfigParser
import lomination.powerrules.lexing.Lexer
import lomination.powerrules.formatting.Formatter
import lomination.powerrules.macros.{MacroApplier, MacroParser}
import lomination.powerrules.powerrulesparsing.PowerRulesParser
import lomination.powerrules.writing.Writer
import lomination.powerrules.util.style.{ansi, ansi0}
import scala.util.{Success, Try}

// @formatter:off

object Compiler:

  val logger = org.log4s.getLogger

  def apply(code: String): Try[String] =
    for {
      _ <- Success(())
      (configIn, macroIn, rulesIn) = section(code)
      _ = logger info s"${ansi(34, 1)}Parsing config section started$ansi0"
      config <- ConfigParser(configIn)
      _ = logger info s"${ansi(32, 1)}Parsing config section succeeded$ansi0"
      _ = logger info s"${ansi(34, 1)}Lexing macro section started$ansi0"
      macroTk <- Lexer(macroIn)(using config)
      _ = logger info s"${ansi(32, 1)}Lexing macro section succeeded$ansi0"
      _ = logger info s"${ansi(34, 1)}Formatting tokens from rules section started$ansi0"
      fmtMacroTk <- Formatter(macroTk)(using config)
      _ = logger info s"${ansi(32, 1)}Formatting tokens from rules section succeeded$ansi0"
      _ = logger info s"${ansi(34, 1)}Parsing macro section started$ansi0"
      macros <- MacroParser(fmtMacroTk)
      _ = logger info s"${ansi(32, 1)}Parsing macro section succeeded$ansi0"
      _ = logger info s"${ansi(34, 1)}Lexing rules section started$ansi0"
      rulesTk <- Lexer(rulesIn)(using config)
      _ = logger info s"${ansi(32, 1)}Lexing rules section succeeded$ansi0"
      _ = logger info s"${ansi(34, 1)}Macro applying started$ansi0"
      appliedRulesTk <- MacroApplier(rulesTk, macros)
      _ = logger info s"${ansi(32, 1)}Macro applying succeeded$ansi0"
      _ = logger info s"${ansi(34, 1)}Formatting tokens from rules section started$ansi0"
      fmtRulesTk <- Formatter(appliedRulesTk)(using config)
      _ = logger info s"${ansi(32, 1)}Formatting tokens from rules section succeeded$ansi0"
      _ = logger info s"${ansi(34, 1)}Parsing rules section started$ansi0"
      ruleFile <- PowerRulesParser(fmtRulesTk)
      _ = logger info s"${ansi(32, 1)}Parsing rules section succeeded$ansi0"
      _ = logger info s"${ansi(34, 1)}Writing rules started$ansi0"
      written = Writer(ruleFile)(using config)
      _ = logger info s"${ansi(32, 1)}Writing rules succeeded$ansi0"
      _ = logger info s"${ansi(32, 1)}Compilation succeeded !!!$ansi0"
    } yield written

  def section(code: String): (String, String, String) =
    val formatted =
      """((?:(?://[^\n]*\n| *\n)*\n)?)((?:::[cC][oO][nN][fF][iI][gG]:: *\n[\S\s]*?\n)?)((?:::[mM][aA][cC][rR][oO][sS]?:: *\n[\S\s]*?\n)?)(::[rR][uU][lL][eE][sS]?:: *\n[\S\s]+)""".r
    code match
      case formatted(empty, rawCfg, rawMac, rawRul) =>
        logger debug "Compiling formatted powerrules"
        val lnB4Cfg = empty.count(_ == '\n')
        val lnB4Mac = rawCfg.count(_ == '\n') + lnB4Cfg
        val lnB4Rul = rawMac.count(_ == '\n') + lnB4Mac
        (
          if rawCfg.isEmpty then "" else "\n" * lnB4Cfg + rawCfg.dropWhile(_ != '\n'),
          if rawMac.isEmpty then "" else "\n" * lnB4Mac + rawMac.dropWhile(_ != '\n'),
          if rawRul.isEmpty then "" else "\n" * lnB4Rul + rawRul.dropWhile(_ != '\n')
        )
      case str =>
        logger debug "Compiling non-formated powerrules (fast mode)"
        ("", "", str)
