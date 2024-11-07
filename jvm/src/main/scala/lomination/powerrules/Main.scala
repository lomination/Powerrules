package lomination.powerrules

import scala.io.Source
import scala.util.Using
import java.io.PrintWriter
import lomination.powerrules.Compiler

@main
def main(fileName: String) =

  val extRegex = """^[\S\s]+?\.(?:[pP][oO][wW][eE][rR][rR][uU][lL][eE][sS]|[tT][xX][tT])$"""

  val newFileName =
    if (fileName.matches(extRegex))
      fileName.replaceAll("""\.[^.]+$""", ".rules")
    else
      fileName + ".rules"

  val result = for {
    input  <- Using(Source.fromFile(fileName))(_.mkString)
    output <- Compiler(input)
    _      <- Using(new PrintWriter(newFileName))(_.write(output))
  } yield ()
  result.get
