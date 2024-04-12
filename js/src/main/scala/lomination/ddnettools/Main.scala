package lomination.ddnettools

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import org.scalajs.dom
import scalatags.Text.all.*
import org.scalajs.dom.HTMLTextAreaElement
import org.scalajs.dom.HTMLButtonElement
import scala.util.Success
import lomination.ddnettools.writers.Writable
import lomination.ddnettools.RuleFile
import lomination.ddnettools.parser.MyParser
import lomination.ddnettools.writers.BasicWriter.{given Writable[RuleFile]}
import scala.util.Failure

@main
def main: Unit =
  dom.document.addEventListener(
    "DOMContentLoaded",
    { _ =>
      dom.document.querySelector("#app").innerHTML = div(
        h1("Welcome to ddnet autorule converter"),
        textarea(id := "input", rows := 10, cols := 50),
        br,
        button(id := "convert", "convert"),
        br,
        textarea(id := "output", rows := 10, cols := 50)
      ).toString

      val input   = dom.document.getElementById("input").asInstanceOf[HTMLTextAreaElement]
      val output  = dom.document.getElementById("output").asInstanceOf[HTMLTextAreaElement]
      val convert = dom.document.getElementById("convert").asInstanceOf[HTMLButtonElement]
      val parser  = MyParser()
      convert.addEventListener(
        "click",
        { _ =>
          parser(input.value) match
            case Success(autorule) => output.value = autorule.write(using autorule.defTile)
            case Failure(error)    => output.value = s"ERROR: $error"
        }
      )
    }
  )