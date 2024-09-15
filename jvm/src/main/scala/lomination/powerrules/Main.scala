package lomination.powerrules

import scala.io.Source
import scala.util.Using
import java.io.PrintWriter
import lomination.powerrules.Compiler

@main
// def main(fileName: String) =
def main =
  val fileName = "exemple.txt"
  val result = for {
    input  <- Using(Source.fromFile(fileName))(_.mkString)
    output <- Compiler(input)
    _      <- Using(new PrintWriter("exemple.rules"))(_.write(output))
  } yield ()

  result.get
