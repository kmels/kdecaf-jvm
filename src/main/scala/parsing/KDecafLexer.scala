package parsing

import scala.util.parsing.combinator.lexical.StdLexical
import scala.collection.mutable.HashSet

class KDecafLexer extends StdLexical{
//  override def token:Parser[Token] = super.token 
  
  reserved ++= Set("class","struct","true","false","void","if","else","while","return","int","char","boolean")

  override val delimiters = new HashSet[String] ++ Set("{","}",";","[","]")  
}
