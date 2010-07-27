package compiler.parsing.test

import org.scalatest.FunSuite
import compiler.parsing.ast._

/**
 * Parsing tests for Statements
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 1.0
 */ 

class StatementTests extends ParserTest[Statement] with FunSuite{
  val parser:PackratParser[Statement] = statement

  test("If statements 1") {
    input = """if (exp) {
      exp;
    }"""
    accept (input)
    
    input = """if (exp){
      method1();
    }      
    else{
      method2();
    }"""
    accept(input)
  }

  test("while statements"){
    input = """
      while(exp){
	if (exp2){
	  method3();
	}
      }
    """
    accept(input)
  }

  test("return statements"){
    //return statement
    accept("return id[2];")
    accept("return exp.member;")
  }
    
  test("assignment"){
    accept("x = b")
  }
   
  test("misc statements"){
    input = """{
      struct s1 id2;
      int a1;
    
      struct s1{
	int m;
      } id [20];
      
      if (s1.m == s2.m){
	m();
      }
    }"""
    accept(input)
  }
}
