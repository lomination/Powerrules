package lomination.powerrules.writing

import lomination.powerrules.ast.Comment
import lomination.powerrules.config.Config
import lomination.powerrules.writing.Writer.given_Writable_Comment.write
import lomination.powerrules.FunSuite

class WriterTest extends FunSuite {

  test("WriterTest - write comment") {
    val ast      = Comment(" hello")
    val test     = ast.write(using Config.default)
    val expected = "# hello\n"
    assert(clue(test) == clue(expected))
  }

}
