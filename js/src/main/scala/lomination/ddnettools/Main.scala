package lomination.ddnettools

import scala.util.{Success, Failure}
import scala.scalajs.js
import scala.scalajs.js.annotation.*

import org.scalajs.dom
import org.scalajs.dom.{HTMLTextAreaElement, HTMLButtonElement, HTMLSpanElement}
import scalatags.Text.all.*
import lomination.ddnettools.RuleFile
import lomination.ddnettools.build.BuildInfo
import lomination.ddnettools.parser.MyParser
import lomination.ddnettools.writers.Writable
import lomination.ddnettools.writers.BasicWriter.{given Writable[RuleFile]}

@main
def main: Unit =
  dom.document.addEventListener(
    "DOMContentLoaded",
    { _ =>
      val version = dom.document.getElementById("version").asInstanceOf[HTMLSpanElement]
      version.textContent = BuildInfo.version

      val input   = dom.document.getElementById("input").asInstanceOf[HTMLTextAreaElement]
      val output  = dom.document.getElementById("output").asInstanceOf[HTMLTextAreaElement]
      val convert = dom.document.getElementById("convert").asInstanceOf[HTMLButtonElement]
      val parser  = MyParser()
      convert.addEventListener(
        "click",
        { _ =>
          parser(input.value) match
            case Success(autorule) => output.value = autorule.write(using autorule.defTile)
            case Failure(error)    => output.value = s"ERROR: ${error.getMessage}"
        }
      )

      val copy = dom.document.getElementById("copy").asInstanceOf[HTMLButtonElement]
      copy.addEventListener(
        "click",
        _ => dom.window.navigator.clipboard.writeText(output.value)
      )
    }
  )
