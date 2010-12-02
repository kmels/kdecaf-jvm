package kmels.uvg.kdecaf.compiler.codegen

/**
 * Intermediate code
 *
 * @author Carlos Lopez
 * @version 3.0
 * @since 3.0
 */

object JVMTypes extends Enumeration{
  case class JVMType(val mkString:String, val id:Int, val jasminType:String) extends Value{
    override def toString = mkString
  }
  
  val INT = JVMType("int",1,"I")
  val VOID = JVMType("void",2,"V")
  val STRING_ARGS = JVMType("String",3,"[Ljava/lang/String")
}

object Constants extends Enumeration{
  case class ConstantField[T](val constant:T,val id:Int) extends Value with AnyValue{
    override def toString = constant.toString
    val lastEvaluatedField = null
    val load = NMenomics.loadValue(constant)
    val store = "???? istore_"+this.toString
  }

  val ZERO = ConstantField[Int](constant=0,id=1)
  val NULL = ConstantField[Any](constant=0,2)
  val NOTHING = ConstantField[Any](None,3)
  
}
import JVMTypes._

trait ClassMember{ def jasminCode(implicit tabSpaces:Int):String }

trait Statement{
  def jasminCode(implicit tabSpaces:Int):String
}

trait AnyValue extends Body{
  val computations = Nil
  val load:String
  val store:String
}

case class StringValue(val string:String) extends AnyValue{
  val lastEvaluatedField = null
  val load:String = "ldr "+string
  val store:String = "store "+string
}

trait Field extends AnyValue  {
  val index: Int
  val lastEvaluatedField = this
  val store:String
  val load:String

  println("CREO FIELD "+index)
}

case class GlobalField(val index:Int) extends Field{ 
  override def toString = "g["+index+"]" 
  val load:String = NMenomics.loadField(index)
  val store:String = NMenomics.storeField(index)
}

case class LocalField(val index:Int) extends Field{ 
  override def toString = "f["+index+"]" 
  val load:String = NMenomics.loadField(index)
  val store:String = NMenomics.storeField(index)
}

case class GotoStatement(val evaluatedField:Field, val addressLabel:Label) extends Statement{
  override def toString = "goto "+addressLabel+" if "+evaluatedField.toString

  def jasminCode(implicit tabSpace:Int) = StringBuilder(
    evaluatedField.load,
    "ifeq "+addressLabel
  )

}

case class PrintLineStatement(val line:String) extends Statement{
  override def toString = "print "+line

  def jasminCode(implicit tabSpace:Int):String = 
    StringBuilder(
      "getstatic java/lang/System/out Ljava/io/PrintStream;",
      "ldc \""+line+"\"",
      "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V"
    )
}

case class PrintIntegerStatement(val field:Field) extends Statement{
  override def toString = "print "+field.toString

  def jasminCode(implicit tabSpace:Int):String = 
    StringBuilder(
      "getstatic java/lang/System/out Ljava/io/PrintStream;",
      field.load,
      "invokevirtual java/io/PrintStream/println(I)V"
    )
}

trait Assignment extends ClassMember with Statement{
  val assignedVar:Field
}

case class TwoAddressAssignment(val assignedVar:Field,var1:AnyValue) extends Assignment{ //e.g. f[0] = Constants.ZERO or f[0] = g[1]
  override def toString = assignedVar + " = "+var1

  def jasminCode(implicit tabSpace:Int):String = assignedVar match{
    case GlobalField(index) => {
      StringBuilder(".field public g_"+index+" I")
    }
    case LocalField(index)=>{
      StringBuilder(var1.load,
		    assignedVar.store)
    }
  }
}
case class ThreeAddressAssignment(val assignedVar:Field,var1:Field,var2:Field,operator:Operators.Operator)  extends Assignment{
  override def toString = assignedVar + " = "+var1 +" "+operator.sign+" "+var2

  def jasminCode(implicit tabSpaces:Int):String = 
    StringBuilder(var1.load,
		  var2.load,
		  operator.jvmMnemonic)
}
		
case class Return(t: JVMType, val returnField:Option[Field]) extends Body with Statement{
  val computations = Nil
  val lastEvaluatedField = returnField match{
    case Some(field) => field
    case _ => null
  }

  override def toString = {
    val returnFieldString = returnField match{
      case Some(f) => f.toString
      case _ => ""
    }
    
    t.mkString.substring(0,1)+"return "+returnFieldString
  }

  def jasminCode(implicit tabSpaces:Int) = {
    val returnString = t match {
      case JVMTypes.INT => "ireturn"
      case JVMTypes.VOID => "return"      
    }

    StringBuilder(returnString)
  }
} 

case class Parameter(t : JVMType, index: Int){
  override def toString = t.toString
  def jasminType = t.jasminType
}

object Operators extends Enumeration{
  trait Operator{ 
    val sign:String 
    val jvmMnemonic:String
  }
  
  case class FieldOperator(val sign:String,val jvmMnemonic:String) extends Value with Operator { def id:Int = 
    sign.map(_.toInt).reduceLeft(_+_)
  } 

  val ADDITION = FieldOperator("+","iadd")
  val SUBSTRACTION = FieldOperator("-","isubstract")
  val MULTIPLICATION = FieldOperator("*","imul")
  val DIVISION = FieldOperator("/","idiv")
  val MOD = FieldOperator("%","irem")
  val AND = FieldOperator("&&","ifeq")
  val OR = FieldOperator("||","ifeq")
  val LESSEQUALSTHAN = FieldOperator("<=","if_cmple")
  val LESSTHAN = FieldOperator("<","if_icmplt")
  val GREATERTHAN = FieldOperator(">","if_icmpgt")
  val GREATEREQUALSTHAN = FieldOperator(">=","if_icmpge")
  val EQUALS = FieldOperator("==","ifeq")
  val NEQUALS = FieldOperator("!=","ifeq")
}

case class Method(returnType: JVMType, name:String, var params: List[Parameter], body: MethodBody) extends ClassMember{
  val bodyString = "\n\t"+body
  
  override def toString = returnType.toString +" "+name+"("+params.map(_.toString).mkString(", ")+")" + bodyString
  
  def jasminCode(implicit tabSpace:Int):String = {
    val extraParam:List[Parameter]= name match{
      case "main" => List(Parameter(JVMTypes.STRING_ARGS,params.size))
      case _ => Nil
    }
    
    val returnStatement:String = body.computations.last match{
      case r:Return => ""
      case _ => 
	if (returnType==JVMTypes.VOID)
	    "\nreturn"
	else
	    ""
    }

    
    val signature = StringBuilder(
      ".method public static "+name + StringBuilder.parameters(extraParam++params)+returnType.jasminType 
    )(tabSpace + 1)
    
    val bodyCode = body.jasminCode(tabSpace+2)

    StringBuilder(
      signature,
      bodyCode,
      returnStatement,
      ".end method\n"
    )//+"\n"+bodyCode+"\n"+returnStatement+"\n"+".end method"
    
//    ".method public static "+name+"("+(extraParam++params).map(_.jasminType).mkString("",";",";")+")"+returnType.jasminType+"\n.limit stack 2000\n"+body.jasminCode+returnStatement+"\n.end method  \n"
  }
}

trait Body{
  val lastEvaluatedField:Field
  val computations:List[Statement]  
}

case class Label(val labelName:String) extends Statement{
  override def toString = labelName;

  def jasminCode(implicit tabSpace:Int) = StringBuilder("Pendiente #LABEL")
}

case class MethodBody(val computations: List[Statement]) extends Body{
  override def toString = computations.map(_.toString).mkString("\n\t")
  
  def getLastFromStatement(statement:Statement):Field = statement match{
    case assignment: Assignment => assignment.assignedVar
    case returnStatement : Return => lastEvaluatedField
    case printStatement : PrintLineStatement => null
    case printInteger : PrintIntegerStatement => null
    case label : Label => null
  }

  val lastEvaluatedField:Field = computations match{
    case head::Nil => getLastFromStatement(head)
    case head::tail => getLastFromStatement((computations).last)
    case Nil => null
  }

  def jasminCode(implicit tabSpace:Int):String = StringBuilder(

    (List(".limit stack 2000",
      ".limit locals 10")++computations.map(_.jasminCode)) :_*
    )
}

object DummyBody{
  def apply(p:String):MethodBody = new MethodBody(Nil){
    override val lastEvaluatedField = LocalField(-1)
  }
}

object StringBuilder{
  def apply(xs:String*)(implicit tabSpaces:Int):String = fromList(xs.toList)

  def parameters(params:Seq[Parameter]) = "("+params.map(_.jasminType).mkString("",";",";")+")"

  private def fromList(xs:List[String])(implicit tabSpaces:Int):String = xs.foldLeft(tab)((a,b) => a+"\n"+tab+b)

  private def tab(implicit tabSpaces:Int):String = (for (t <- 1 to tabSpaces) yield "\t").mkString
}

object NMenomics {
  def pushConstant[T](x:T) = x match{
    case int:Int => if (int < 6) "iconst_"+int else "bipush "+int
  }

  def storeField(index:Int):String = 
    if (index < 4) "istore_"+index else "istore "+index


  def loadField(index:Int) = 
    if (index < 4) "iload_"+index else "iload "+index

  def loadValue[T](value:T) = value match{
    case int:Int => if (int < 4) "iconst_"+int else "bipush "+int
    case wtf => "WTF???: "+wtf
  }


}
