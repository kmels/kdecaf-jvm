package compiler.parsing.test

import org.scalatest.FunSuite
import compiler.parsing.ast._

/**
 * Parsing tests for expressions
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 1.0
 */ 
class ExpressionTests extends ParserTest[Expression] with FunSuite{
  val parser = expression
  implicit def intLiteral(i:Int):IntLiteral = IntLiteral(i)
  implicit def charLiteral(c:Char):CharLiteral = CharLiteral(c)
  implicit def boolLiteral(b:Boolean):BoolLiteral = BoolLiteral(b)

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

  test("Literals"){
    input = "02"
    result must be (Some(IntLiteral(2)))
    
    input = "'c'"
    result must be (Some(CharLiteral('c')))

    input = "true"
    result must be (Some(BoolLiteral(true)))
  }

  test("Operators precedence"){    
    input = "1+1"
    result must be (Some(ExpressionAdd(
      1,1
    )))
    
    input = "-1*(2+3)"
    result must be (Some(ExpressionMult(
      NegativeExpression(1),ExpressionAdd(2,3)
    )))

    input = "1+2-3"
    result must be (Some(ExpressionAdd(
      1,ExpressionSub(2,3)
    )))

    input = "4+5*6"
    result must be (Some(ExpressionAdd(
      4,ExpressionMult(5,6)      
    )))
    
    input = "7*8"
    result must be (Some(ExpressionMult(
      7,8
    )))

    input = "9%10"
    result must be (Some(ExpressionMod(
      9,10
    )))

    input = "11*12+13"
    result must be (Some(ExpressionAdd(      
	ExpressionMult(11,12),13
    )))

    input = "14*15%16"
    result must be (Some(ExpressionMult(
      14,ExpressionMod(15,16)
    )))
    
    input = "1+2%3"
    result must be (Some(ExpressionAdd(
      1,ExpressionMod(2,3)
    )))

    input = "(20+21)/22"
    result must be (Some(ExpressionDiv(
      ExpressionAdd(20,21),22
    )))
    
    accept("exp1 % exp")
    accept("id[pos].member % id2[pos2]")
    accept("e+e+e*2+1*(e+2/2)")
    accept("exp1 % exp2")
    accept("exp * exp + exp")
    accept("(exp1+exp2)*exp3")
  }

  test("Operator precedence with bools"){    
    input = "true || false"
    result must be (Some(ExpressionOr(
      true,false
    )))
    
    input = "1+2 <= 5"
    result must be (Some(ExpressionLessOrEquals(
      ExpressionAdd(1,2),5
    )))

    accept("exp1 <= exp2")
    accept("exp == exp")
    accept("exp != exp2")
  }
}
