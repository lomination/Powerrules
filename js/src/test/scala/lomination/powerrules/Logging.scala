package lomination.powerrules

import org.log4s.Log4sConfig.setLoggerThreshold
import org.log4s.OffThreshold

/** This class is a wrapper of `munit.FunSuite`. In ScalaJVM, this class does not add anything to the original implementation. In ScalaJS, this class
  * configures the logging before launching test defined within.
  */
class FunSuite extends munit.FunSuite {
  override def beforeAll(): Unit = setLoggerThreshold("", OffThreshold)
}
