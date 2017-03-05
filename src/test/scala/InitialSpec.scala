/**
  * Created by buck on 3/4/17.
  */

import interpreter._
import org.scalatest.FunSpec
import parser.Parser


class InitialSpec extends FunSpec  {
  describe("Evaluating expressions") {
    it ("can do basics") {
      val expr = MethodCallExpr(LiteralExpr(BlInt(3)), "+", List(LiteralFunctionArgument(LiteralExpr(BlInt(5)))))

      assert(expr.eval(GlobalScope) == BlInt(8))

      val exprParser = Parser.nakedExpr

      val parsedExpr = exprParser.parse("range([7,8,9].size).size").get.value

      assert(parsedExpr.eval(GlobalScope) == BlInt(3))
    }
  }

  describe("Running modules") {
    it("works") {

    }
  }
}