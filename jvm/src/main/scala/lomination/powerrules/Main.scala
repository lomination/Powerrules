package lomination.powerrules

import scala.io.Source
import scala.util.Using
import java.io.PrintWriter
import lomination.powerrules.parser.GlobalParser
import lomination.powerrules.writer.Writable
import lomination.powerrules.writer.BasicWriter.{given Writable[RuleFile]}

@main
def main =
  val parser = GlobalParser()
  val result = for {
    input    <- Using(Source.fromFile("example.txt"))(_.mkString)
    ruleFile <- parser(input)
    _        <- Using(new PrintWriter("example.rules"))(_.write(ruleFile.write(using ruleFile.tmpTile)))
  } yield ()

  result.failed.foreach(println)
