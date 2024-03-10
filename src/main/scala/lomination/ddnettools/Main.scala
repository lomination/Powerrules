package lomination.ddnettools

import parser.AutoruleParser
import scala.util.Using
import scala.io.Source
import java.io.PrintWriter
import lomination.ddnettools.writers.Writable
import lomination.ddnettools.writers.BasicWriter.{given Writable[Autorule]}

@main
def main =
  val parser = AutoruleParser()
  val result = for {
    input    <- Using(Source.fromFile("file.txt"))(_.mkString)
    autorule <- parser(input)
    _        <- Using(new PrintWriter("test.rules"))(_.write(autorule.write))
  } yield ()

  result.failed.foreach(println)
