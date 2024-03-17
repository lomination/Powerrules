package lomination.ddnettools

import scala.util.Using
import scala.io.Source
import java.io.PrintWriter
import lomination.ddnettools.parser.MyParser
import lomination.ddnettools.writers.Writable
import lomination.ddnettools.writers.BasicWriter.{given Writable[Autorule]}

@main
def main =
  val parser = MyParser()
  val result = for {
    input    <- Using(Source.fromFile("example.txt"))(_.mkString)
    autorule <- parser(input)
    _        <- Using(new PrintWriter("example.rules"))(_.write(autorule.write(using autorule.tmp)))
  } yield ()

  result.failed.foreach(println)
