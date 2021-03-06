package kmels.uvg.kdecaf.compiler.parsing.test

import org.scalatest.FunSuite
import kmels.uvg.kdecaf.compiler.parsing.ast.{Literal,IntLiteral,CharLiteral,BoolLiteral}

/**
 * Parsing tests for Literal
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 1.0
 */ 
class LiteralTests extends ParserTest[Literal[_]] with FunSuite{
  val parser = literal

  test ("Integer literals"){
    input = "12345"
    result must be (Some(IntLiteral(12345)))

    input = "0"
    result must be (Some(IntLiteral(0)))
  }

  test ("Boolean literals"){
    input = "true"
    result must be (Some(BoolLiteral(true)))

    input = "false"
    result must be (Some(BoolLiteral(false)))

    input = "False"
    result must be (None)
  }

  test ("Char literals"){
    input = "'e'"
    result must be (Some(CharLiteral('e')))

    input = "'m'"
    result must be (Some(CharLiteral('m')))
  }
}
