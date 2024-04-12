package lomination.ddnettools.parser

import scala.util.{Try, Success, Failure}
import org.log4s.*

case class Macro(name: String, paramNames: Seq[String], content: String):
  val logger = getLogger
  def apply(paramValues: Seq[String]): Try[String] =
    val vLength = paramValues.size
    val nLength = paramNames.size
    if (vLength != nLength)
      val msg = s"Invalid number of params for macro `$name`. Given: $vLength, expected: $nLength"
      val exception = IllegalArgumentException(msg)
      logger.error(msg)
      logger.debug(s"Given params: `${paramValues.mkString("`, `")}`")
      Failure(exception)
    else
      val res = (paramNames zip paramValues)
        .foldLeft(content) { case (c, (n, v)) => c.replace(s"<$n>", v) }
      Success(res)
