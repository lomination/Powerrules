package lomination.powerrules

import scala.util.{Failure, Success}
import org.scalajs.dom
import org.scalajs.dom.{HTMLButtonElement, HTMLSpanElement, HTMLTextAreaElement}
import lomination.powerrules.build.BuildInfo

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
          Compiler(input.value) match
            case Success(value) => output.value = value
            case Failure(error) => output.value = s"ERROR: ${error.getMessage}"
        }
      )

      val copy = dom.document.getElementById("copy").asInstanceOf[HTMLButtonElement]
      copy.addEventListener(
        "click",
        _ => dom.window.navigator.clipboard.writeText(output.value)
      )
    }
  )
