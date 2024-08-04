package lomination.powerrules

import org.log4s.OffThreshold
import org.log4s.Log4sConfig.setLoggerThreshold

class FunSuite extends munit.FunSuite {
  override def beforeAll(): Unit = setLoggerThreshold("", OffThreshold)
}
