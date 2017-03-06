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

//    case class LiteralExpr(blValue: BlValue) extends BlExpression
//    case class MethodCallExpr(calleeExpr: BlExpression,
//                              name: String,
//                              args: List[BlFunctionArgument]) extends BlExpression
//    case class FunctionCallExpr(functionExpr: BlExpression, args: List[BlFunctionArgument]) extends BlExpression
//    case class FieldAccessExpr(callee: BlExpression, name: String) extends BlExpression
//    case object ThisExpr extends BlExpression
//    case class NameExpr(name: String) extends BlExpression
//    case class FunctionDefinitionExpr(args: ArrayDestructurePattern,
//                                      defaultVals: Map[String, BlValue],
//                                      body: List[BlStatement]) extends BlExpression

    it ("can do literal exprs" ) {
      assert(parse("123").isInstanceOf[LiteralExpr])
    }

    it ("can do array literals") {
      val expr = parse("[1, 2, 3]").asInstanceOf[FunctionCallExpr]
      assert(expr.functionExpr == LiteralExpr(BlListClass))
    }

    it ("can do map literals") {
      val expr = parse("{a: 1}").asInstanceOf[MethodCallExpr]
    }

    it ("can do string literals") {
      val expr = parse("\"foobar\"").asInstanceOf[LiteralExpr]
    }

    it ("can do method calls") {
      val expr = parse("foo.bar()").asInstanceOf[MethodCallExpr]
      assert(expr.calleeExpr == NameExpr("foo"))
      assert(expr.name == "bar")
      assert(expr.args.isEmpty)

      assert(parse("y + z(foo, bar, zaz(4))").isInstanceOf[MethodCallExpr])
    }

    it ("can do function calls") {
      assert(parse("foo(bar)(baz)").isInstanceOf[FunctionCallExpr])
    }

    it ("can do field accesses") {
      assert(parse("a.b().c").isInstanceOf[FieldAccessExpr])
    }

    it ("can do this expr") {
      assert(parse("this") == ThisExpr)
    }

    it ("can do name expr") {
      assert(parse("foo").asInstanceOf[NameExpr].name == "foo")
    }

    it ("can do function definition exprs") {
      assert(parse("(x, y) => x + y").isInstanceOf[FunctionDefinitionExpr])
      assert(parse("(x, y) => { val z = x + y; return x + z * z; }").isInstanceOf[FunctionDefinitionExpr])
      assert(parse("(x) => { val z = x + y; return x + z * z; }").isInstanceOf[FunctionDefinitionExpr])
      assert(parse("(x) => { if (x == 0) { return 1; } else { return x * factorial(x - 1); } }").isInstanceOf[FunctionDefinitionExpr])
    }
  }

  describe("Parsing statements") {

    val stmtParser = Parser.nakedStmt
    def parse(string: String): BlStatement = {
      stmtParser.parse(string).get.value
    }

    it("can do val decl statements") {
      assert(parse("val x = y;").isInstanceOf[ValDeclStatement])
      assert(parse("val x = y + z(foo, bar, zaz(4));").isInstanceOf[ValDeclStatement])
      assert(parse("val [z, q] = foo;").isInstanceOf[ValDeclStatement])
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

    it("can do if statements") {
      assert(parse("if(something) { y(sdfsdf) + fds; } ").isInstanceOf[IfStatement])
      assert(parse("if(something) { y(sdfsdf) + fds; } else { foo; bar; bz; }").isInstanceOf[IfStatement])
    }
  }


}
