package parsing.test

import org.scalatest.FunSuite
import parsing.ast._

/**
 * Parsing tests for expressions
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 1.0
 */ 
class ExpressionTests extends ParserTest[Expression] with FunSuite{
  val parser = expression

  
  test("Method calls"){
      input = "id()"
      result must be (Some(MethodCall("id",List())))

      input = "id(p1, p2, p3[index])"
      result must be (Some(
	MethodCall("id",List(
	  SimpleLocation("p1"),
	  SimpleLocation("p2"),
	  ArrayLocation("p3",SimpleLocation("index"))
	))
      ))      
  }
}
