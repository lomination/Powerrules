package lomination.ddnettools.parser

import scala.util.{Try, Success, Failure}
import org.log4s.*

case class Macro(name: String, paramNames: Seq[String], content: String):
  if (paramNames == Seq(""))
    logger.warn(s"Macro $name is defined with one parameter named with empty string ``.\n" +
      "Consider not using brackets after its definition if no paramter is expected.")
  if (paramNames.sizeIs != paramNames.toSet.size)
    logger.warn(s"Some parameters of macro $name have the same name.")
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
