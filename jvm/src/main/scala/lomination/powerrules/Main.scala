package lomination.powerrules

import java.io.PrintWriter
import scala.io.Source
import scala.util.Using

@main
def main(fileName: String) =

  val newFileName =
    if (fileName.toLowerCase.endsWith(".powerrules"))
      fileName.slice(0, fileName.length - 11) + ".rules"
    else
      fileName + ".rules"

  val result = for {
    input  <- Using(Source.fromFile(fileName))(_.mkString)
    output <- Compiler(input)
    _      <- Using(new PrintWriter(newFileName))(_.write(output))
  } yield ()
  result.get
