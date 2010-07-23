package parsing.test

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import scala.util.parsing.combinator.{lexical,syntactical}
import lexical.Scanners
import syntactical.StandardTokenParsers
import parsing.{KDecafLexer,KDecafParser}

/**
 * A trait for testing parsers
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 1.0
 */ 

trait ParserTest[T] extends KDecafParser with MustMatchers{
  var input:String = ""
  val parser:Parser[T]

  def parseResult:ParseResult[T] = {
    val tokens = new lexical.Scanner(input)
    parser(tokens)
  }

  def result:Option[T] = parseResult match{
    case Success(ast,_) => Some(ast)
    case _ => None
  }

  def success:Boolean = result match{
    case Some(r) => true
    case _ => false
  }
}

trait SuccessfulParserTest[T] extends ParserTest[T] {
  
}
