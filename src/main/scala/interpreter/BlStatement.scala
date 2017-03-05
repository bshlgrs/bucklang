package interpreter

/**
  * Created by buck on 3/4/17.
  */

import scala.util.control.Breaks._

abstract class BlStatement {
  def eval(scope: BlScope): Option[BlValue] = this match {
    case ValDeclStatement(lhs, rhs) => scope.declareVals(lhs.destructure(rhs.eval(scope)).get) ; None
    case VarDeclStatement(lhs, rhs) => scope.declareVar(lhs, rhs.eval(scope)); None
    case VarSetStatement(lhs, rhs) => scope.set(lhs, rhs.eval(scope)); None
    case FieldSetStatement(lhs, field, rhs) => lhs.eval(scope).setField(field, rhs.eval(scope)) ; None
    case ExprStatement(exp) => exp.eval(scope); None
    case ReturnStatement(value) => Some(value.map(_.eval(scope)).getOrElse(BlNull))
    case WhileStatement(cond, body) => {
      if (cond.eval(scope).truthy) {
        BlStatement.evalBlock(scope, body)
      } else {
        None
      }
    }
    case IfStatement(cond, thenCase, elseCase) => {
      if (cond.eval(scope).truthy) {
        BlStatement.evalBlock(scope, thenCase)
      } else {
        BlStatement.evalBlock(scope, elseCase)
      }
    }
  }
}

object BlStatement {
  def evalBlock(scope: BlScope, stmts: List[BlStatement]): Option[BlValue] = stmts match {
    case Nil => None
    case s :: ss => {
      s.eval(scope) match {
        case Some(res) => Some(res)
        case None => evalBlock(scope, ss)
      }
    }
  }
}

case class ValDeclStatement(lhs: BlPattern, rhs: BlExpression) extends BlStatement
case class VarDeclStatement(lhs: String, rhs: BlExpression) extends BlStatement
case class VarSetStatement(lhs: String, rhs: BlExpression) extends BlStatement
case class FieldSetStatement(lhs: BlExpression, field: String, rhs: BlExpression) extends BlStatement
case class ExprStatement(exp: BlExpression) extends BlStatement
case class ReturnStatement(value: Option[BlExpression]) extends BlStatement
case class WhileStatement(cond: BlExpression, body: List[BlStatement]) extends BlStatement
case class IfStatement(cond: BlExpression, thenCase: List[BlStatement], elseCase: List[BlStatement]) extends BlStatement
