package lomination.powerrules

import scala.util.{Success, Failure}
import org.scalajs.dom
import org.scalajs.dom.{HTMLTextAreaElement, HTMLButtonElement, HTMLSpanElement}
import lomination.powerrules.RuleFile
import lomination.powerrules.build.BuildInfo
import lomination.powerrules.parsing.GlobalParser
import lomination.powerrules.writing.Writable
import lomination.powerrules.writing.BasicWriter.{given Writable[RuleFile]}

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
      convert.addEventListener(
        "click",
        { _ =>
          GlobalParser(input.value) match
            case Success(autorule) => output.value = autorule.write(using autorule.tmpTile)
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
