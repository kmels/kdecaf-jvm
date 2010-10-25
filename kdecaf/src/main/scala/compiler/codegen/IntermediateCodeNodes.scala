package kmels.uvg.kdecaf.compiler.codegen

/**
 * Intermediate code
 *
 * @author Carlos Lopez
 * @version 3.0
 * @since 3.0
 */

object JVMTypes extends Enumeration{
  case class JVMType(val mkString:String, val id:Int) extends Value{
    override def toString = mkString
  }
  
  val INT = JVMType("int",1)
  val VOID = JVMType("void",2)
}

object Constants extends Enumeration{
  case class ConstantField[T](val constant:T,val id:Int) extends Value with AnyValue{
    override def toString = constant.toString
    val lastEvaluatedField = null
  }

  val ZERO = ConstantField[Int](constant=0,id=1)
  val NULL = ConstantField[Any](constant=0,2)
  val NOTHING = ConstantField[Any](None,3)
}
import JVMTypes._

trait ClassMember

trait Statement

trait AnyValue extends Body{
  val computations = Nil
}

trait Field extends AnyValue  {
  val index: Int
  val lastEvaluatedField = this
}

case class GlobalField(val index:Int) extends Field{ override def toString = "g["+index+"]" }
case class LocalField(val index:Int) extends Field{ override def toString = "f["+index+"]" }

case class GotoStatement(val evaluatedField:Field, val addressLabel:Label) extends Statement{
  override def toString = "goto "+addressLabel+" if "+evaluatedField.toString+" is false"
}

trait Assignment extends ClassMember with Statement{
  val assignedVar:Field
}

case class TwoAddressAssignment(val assignedVar:Field,var1:AnyValue) extends Assignment{ //e.g. f[0] = Constants.ZERO or f[0] = g[1]
  override def toString = assignedVar + " = "+var1
}
case class ThreeAddressAssignment(val assignedVar:Field,var1:Field,var2:Field,operator:Operators.Operator)  extends Assignment{
  override def toString = assignedVar + " = "+var1 +" "+operator.sign+" "+var2
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
} 

case class Parameter(t : JVMType, index: Int){
  override def toString = t.toString
}

object Operators extends Enumeration{
  trait Operator{ val sign:String }
  
  case class FieldOperator(val sign:String) extends Value with Operator { def id:Int = 
    sign.map(_.toInt).reduceLeft(_+_)
  } 

  val ADDITION = FieldOperator("+")
  val SUBSTRACTION = FieldOperator("-")
  val MULTIPLICATION = FieldOperator("*")
  val DIVISION = FieldOperator("/")
  val MOD = FieldOperator("%")
  val AND = FieldOperator("&&")
  val OR = FieldOperator("||")
  val LESSEQUALSTHAN = FieldOperator("<=")
  val LESSTHAN = FieldOperator("<")
  val GREATERTHAN = FieldOperator(">")
  val GREATEREQUALSTHAN = FieldOperator(">=")
  val EQUALS = FieldOperator("==")
  val NEQUALS = FieldOperator("!=")
}

case class Method(returnType: JVMType, name:String, params: List[Parameter], body: MethodBody) extends ClassMember{
  val bodyString = "\n\t"+body
  
  override def toString = returnType.toString +" "+name+"("+params.map(_.toString).mkString(", ")+")" + bodyString
}

trait Body{
  val lastEvaluatedField:Field
  val computations:List[Statement]  
}

case class Label(val labelName:String) extends Statement{
  override def toString = labelName;
}

case class MethodBody(val computations: List[Statement]) extends Body{
  override def toString = computations.map(_.toString).mkString("\n\t")
  
  def getLastFromStatement(statement:Statement):Field = statement match{
    case assignment: Assignment => assignment.assignedVar
    case returnStatement : Return => lastEvaluatedField
    case label : Label => null
  }

  val lastEvaluatedField:Field = computations match{
    case head::Nil => getLastFromStatement(head)
    case head::tail => getLastFromStatement((computations).last)
    case Nil => null
  }
}

object DummyBody{
  def apply(p:String):MethodBody = new MethodBody(Nil){
    override val lastEvaluatedField = LocalField(-1)
  }
}
