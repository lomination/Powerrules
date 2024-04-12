package lomination.ddnettools

import scala.io.Source
import scala.util.Using
import java.io.PrintWriter
import lomination.ddnettools.parser.MyParser
import lomination.ddnettools.writers.Writable
import lomination.ddnettools.writers.BasicWriter.{given Writable[RuleFile]}

@main
def main =
  val parser = MyParser()
  val result = for {
    input    <- Using(Source.fromFile("example.txt"))(_.mkString)
    ruleFile <- parser(input)
    _        <- Using(new PrintWriter("example.rules"))(_.write(ruleFile.write(using ruleFile.defTile)))
  } yield ()

  result.failed.foreach(println)
