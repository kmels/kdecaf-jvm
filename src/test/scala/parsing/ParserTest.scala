package parsing.test

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import scala.util.parsing.combinator.{lexical,syntactical}
import lexical.Scanners
import parsing.{KDecafParser}
import parsing.KDecafLexer

/**
 * A trait for testing parsers
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 1.0
 */

trait ParserTest[T] extends KDecafParser with MustMatchers{
  var input:String = ""
  val parser:PackratParser[T]

  def accept = (in:String) => {
    input = in
    val accepted = result match {
      case Some(_) => true
      case _ => false
    }
    accepted must be (true)
  }

  def parseResult:ParseResult[T] = {
    val tokens = new lexical.Scanner(input)
    val packratReader = new PackratReader(tokens)
    parser(packratReader)
  }

  def result[U <: T]:Option[T] = parseResult match{
    case Success(ast,next) => next.atEnd match{
      case true => Some(ast)
      case rest => {
	println("parsed: "+ast)
	println("rest: "+rest)
	None
      }
    }
    case _ => None
  }

  def success:Boolean = result match{
    case Some(r) => true
    case _ => false
  }
}

trait SuccessfulParserTest[T] extends ParserTest[T] {
  
}
