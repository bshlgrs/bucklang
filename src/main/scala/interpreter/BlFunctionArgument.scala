
package interpreter

/**
  * Created by buck on 3/4/17.
  */
abstract class BlFunctionArgument {
  def args(s: BlScope): List[BlValue] = this match {
    case LiteralFunctionArgument(x) => List(x.eval(s))
    case SplatFunctionArgument(x) => x.eval(s) match {
      case BlList(list) => list
      case x => throw new BuckLangException(s"You tried to splat $x, but it is not a list :(")
    }
  }
}

case class LiteralFunctionArgument(value: BlExpression) extends BlFunctionArgument
case class SplatFunctionArgument(value: BlExpression) extends BlFunctionArgument
