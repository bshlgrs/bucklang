package interpreter

/**
  * Created by buck on 3/4/17.
  */
sealed abstract class BlExpression {
  def eval(scope: BlScope): BlValue = this match {
    case LiteralExpr(value) => value
    case MethodCallExpr(calleeExpr, name, args) => {
      val callee = calleeExpr.eval(scope)
      val flattenedArgs = args.flatMap(_.args(scope))

      callee.handleMethodCall(name, flattenedArgs, scope)
    }
    case FunctionCallExpr(functionExpr, args) =>
      functionExpr.eval(scope) match {
        case f: BlCallable => {
          val flattenedArgs = args.flatMap(_.args(scope))

          f.call(flattenedArgs)
        }
        case c: BlClass => {
          val flattenedArgs = args.flatMap(_.args(scope))

          c.construct(flattenedArgs)
        }
        case x => throw new RuntimeException(s"Attempted to call $x as a function")
      }
    case ThisExpr =>
      scope.getThis match {
        case None => throw new RuntimeException("You can't call `this` here")
        case Some(x) => x
      }
    case NameExpr(name) =>
      scope.varsPlusVals.get(name) match {
        case Some(x) => x
        case None => throw new RuntimeException(s"Variable $name is undefined")
      }
    case FieldAccessExpr(callee: BlExpression, name: String) => {
      callee.eval(scope).getField(name)
    }
    case e: FunctionDefinitionExpr => {
      BlFunctionDefinition(e, scope)
    }
  }
}

case class LiteralExpr(blValue: BlValue) extends BlExpression
case class MethodCallExpr(calleeExpr: BlExpression,
                          name: String,
                          args: List[BlFunctionArgument]) extends BlExpression
case class FunctionCallExpr(functionExpr: BlExpression, args: List[BlFunctionArgument]) extends BlExpression
case class FieldAccessExpr(callee: BlExpression, name: String) extends BlExpression
case object ThisExpr extends BlExpression
case class NameExpr(name: String) extends BlExpression
case class FunctionDefinitionExpr(args: ArrayDestructurePattern,
                                  defaultVals: Map[String, BlValue],
                                  body: List[BlStatement]) extends BlExpression

object FunctionDefinitionExpr {
  def lazyWrap(expr: BlExpression) = FunctionDefinitionExpr(ArrayDestructurePattern(Nil, None), Map(), List(ReturnStatement(Some(expr))))
}
