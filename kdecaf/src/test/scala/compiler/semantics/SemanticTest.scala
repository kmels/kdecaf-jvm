package kmels.uvg.kdecaf.compiler.semantics.test

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import kmels.uvg.kdecaf.compiler.{SymbolTable,semantics}
import semantics._

/**
 * A trait for testing semantic rules
 *
 * @author Carlos Lopez
 * @version 2.0
 * @since 2.0
 */

trait SemanticTest[T] extends MustMatchers{
  var symtab: SymbolTable

  val semanticResult: () => SemanticResult
}
