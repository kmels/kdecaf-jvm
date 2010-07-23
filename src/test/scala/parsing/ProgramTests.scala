package parsing.test

import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
import parsing.ast._

/**
 * A parsing test for a Program
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 1.0
 */ 

class ProgramTests extends ParserTest[Program] with Spec{
  val parser = program  

  describe("Simple Program"){
    input = """class Program {
      
    }"""

    it("should have succeeded"){
      parseResult.successful must be (true)
    }

    it("should have zero declarations"){
      result.get.declarations.size must be (0)
    }
  }
}

