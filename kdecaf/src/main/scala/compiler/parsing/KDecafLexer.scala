package kmels.uvg.kdecaf.compiler.parsing

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharSequenceReader.EofCh
import scala.collection.mutable.HashSet

class KDecafLexer extends StdLexical{
  override def token:Parser[Token] = 
    ( identChar ~ rep( identChar | digit )              ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
    | digit ~ rep( digit )                              ^^ { case first ~ rest => NumericLit(first :: rest mkString "") }
    | '\'' ~ (letter|digit) ~ '\''                              ^^ { case '\'' ~ char ~ '\'' => CharLit(char.toString) }
    | '\'' ~ rep( chrExcept('\'', '\n', EofCh) ) ~ '\'' ^^ { case '\'' ~ chars ~ '\'' => StringLit(chars mkString "") }
    | '\"' ~ rep( chrExcept('\"', '\n', EofCh) ) ~ '\"' ^^ { case '\"' ~ chars ~ '\"' => StringLit(chars mkString "") } 
    | EofCh                                             ^^^ EOF
    | '\'' ~> failure("unclosed string literal")        
    | '\"' ~> failure("unclosed string literal")        
    | delim                                             
    | failure("illegal character")
    )
    
  case class CharLit(val chars:String) extends Token {
    override def toString = "'"+chars+"'"
  }

  reserved ++= Set("class","struct","true","false","void","if","else","while","return","int","char","boolean","println")

  override val delimiters = new HashSet[String] ++ Set("{","}",";","[","]","(",")",",","_","'",".","+","-","*","/","%","&&","||","<=","<",">",">=","==","!=","=")
}
