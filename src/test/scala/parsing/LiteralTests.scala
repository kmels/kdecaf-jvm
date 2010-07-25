package parsing.test

import org.scalatest.FunSuite
import parsing.ast.{int,char,boolean,PrimitiveType}

/**
 * Parsing tests for Literal
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 1.0
 */ 
class LiteralTests extends ParserTest[PrimitiveType[_]] with FunSuite{
  val parser = literal
  implicit def intWrapper(i:Int):int = int(i)
  implicit def charWrapper(c:Char):char = char(c)
  implicit def boolWrapper(b:Boolean):boolean = boolean(b)

  test ("Integer literals"){
    input = "12345"
    result must be (Some(int(12345)))

    input = "0"
    result must be (Some(int(0)))
  }

  test ("Boolean literals"){
    input = "true"
    result must be (Some(boolean(true)))

    input = "false"
    result must be (Some(boolean(false)))

    input = "False"
    result must be (None)
  }

  test ("Char literals"){
    input = "'e'"
    result must be (Some(char('e')))

    input = "'m'"
    result must be (Some(char('m')))
  }
}
