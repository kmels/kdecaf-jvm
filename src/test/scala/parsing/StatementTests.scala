package parsing.test

import org.scalatest.FunSuite
import parsing.ast._

/**
 * Parsing tests for Statements
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 1.0
 */ 

class StatementTests extends ParserTest[Statement] with FunSuite{
  val parser:Parser[Statement] = statement

  test("If statement #1") {
    input = """if (exp) {
      exp;
    }"""

    success must be (true)
    
    val expected = new IfStatement(
      SimpleLocation("exp"),
      Block(List(),List(SimpleLocation("exp")))
    )

    //result must be (Some(expected))
  }

  input = "if (1) else (2)"
}
