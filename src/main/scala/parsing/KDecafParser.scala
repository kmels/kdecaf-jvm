package parsing

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import ast._

object KDecafParser extends StandardTokenParsers{  
  override val lexical = new KDecafTokens
}
