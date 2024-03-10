package lomination.ddnettools

import parser.AutoruleParser

@main
def main =
  val inFile = scala.io.Source.fromFile("file.txt")
  val inText = inFile.getLines.mkString("\n")
  val outFile = java.io.File("test.rules")
  val parser = AutoruleParser()
  // outFile = parser(inText)
