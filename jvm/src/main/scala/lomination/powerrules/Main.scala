package lomination.powerrules

import scala.io.Source
import scala.util.Using
import java.io.PrintWriter
import lomination.powerrules.parsing.GlobalParser
import lomination.powerrules.writing.Writable
import lomination.powerrules.writing.BasicWriter.given Writable[RuleFile]

@main
def main =
  val result = for {
    input    <- Using(Source.fromFile("example.txt"))(_.mkString)
    ruleFile <- GlobalParser(input)
    _        <- Using(new PrintWriter("example.rules"))(_.write(ruleFile.write(using ruleFile.tmpTile)))
  } yield ()

  result.failed.foreach(println)
