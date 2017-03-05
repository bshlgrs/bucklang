/**
  * Created by buck on 3/4/17.
  */

import interpreter._
import org.scalatest.FunSpec
import parser.Parser


class ParserSpec extends FunSpec  {
  describe("Parsing expressions") {
    val exprParser = Parser.nakedExpr
    def parse(string: String): BlExpression = {
      exprParser.parse(string).get.value
    }

    it ("can do basics") {
      assert(parse("x + y") == parse("x + (y)"))
      assert(parse("x + y") != parse("x + y + z"))
      assert(parse("x + y * z") == parse("x + (y * z)"))
      assert(parse("x * y + z") == parse("(x * y) + z"))
    }
  }

  describe("Parsing statements") {
//    case ValDeclStatement(lhs, rhs) => scope.declareVals(lhs.destructure(rhs.eval(scope)).get) ; None
//    case VarDeclStatement(lhs, rhs) => scope.declareVar(lhs, rhs.eval(scope)); None
//    case VarSetStatement(lhs, rhs) => scope.set(lhs, rhs.eval(scope)); None
//    case FieldSetStatement(lhs, field, rhs) => lhs.eval(scope).setField(field, rhs.eval(scope)) ; None
//    case ExprStatement(exp) => exp.eval(scope); None
//    case ReturnStatement(value) => Some(value.map(_.eval(scope)).getOrElse(BlNull))
//    case WhileStatement(cond, body) => {
    val stmtParser = Parser.nakedStmt
    def parse(string: String): BlStatement = {
      stmtParser.parse(string).get.value
    }

    it("can do val decl statements") {
      assert(parse("val x = y;").isInstanceOf[ValDeclStatement])
      assert(parse("val x = y + z(foo, bar, zaz(4));").isInstanceOf[ValDeclStatement])
      assert(parse("val [z, q] = y + z(foo, bar, zaz(4));").isInstanceOf[ValDeclStatement])
      assert(parse("val [z, q, ...arg, ff] = y + z(foo, bar, zaz(4));").isInstanceOf[ValDeclStatement])
    }

    it("can do var decl") {
      assert(parse("var x = y + fds;").isInstanceOf[VarDeclStatement])
    }

    it("can do var set") {
      assert(parse("x = y + fds;").isInstanceOf[VarSetStatement])
    }

    it("can do field set") {
      assert(parse("x.y = y + fds;").isInstanceOf[FieldSetStatement])
    }

    it("can do expr statement") {
      assert(parse("y(sdfsdf) + fds;").isInstanceOf[ExprStatement])
    }

    it("can do return statements") {
      assert(parse("return y(sdfsdf) + fds;").isInstanceOf[ReturnStatement])
    }

    it("can do while statements") {
      assert(parse("while(something) { y(sdfsdf) + fds; } ").isInstanceOf[WhileStatement])
    }
  }


}
