package parsing.test

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import scala.util.parsing.combinator.Parsers
import parsing.KDecafParser
import KDecafParser._
import parsing.ast._

class ProgramTests extends Spec with MustMatchers{
  describe("Simple Program"){
    val input = """class Program {
      
    }"""
    
    val parsedResult:ParseResult[Program] = KDecafParser.parse(input)    
    
    val parsedProgram:Program = parsedResult match {
      case Success(ast,_) => ast
      case _ => throw new Exception("")
    }

    it("should have succeeded"){
      parsedResult.successful must be (true)
    }

    it("should have zero declarations"){
      parsedProgram.declarations.size must be (0)
    }
  }
}
