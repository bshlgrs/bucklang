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

  describe("Running blocks") {

    def parseAndRunBlock(blockString: String): BlValue = {
      val parsedCode: List[BlStatement] = Parser.nakedBlockOfStatements.parse(blockString).get.value

      BlStatement.evalBlock(new BlScope(None, Some(GlobalScope)), parsedCode).getOrElse(BlNull)
    }

    it("works") {
      val code =
        """
           val x = range(10);

           return x.size * 5;
        """.stripMargin

      assert(parseAndRunBlock(code) == BlInt(50))
    }


    it("can do function definition") {
      val code =
        """
           val f = (x) => x * 5;

           return f(5);
        """.stripMargin

      assert(parseAndRunBlock(code) == BlInt(25))
    }

    it("can do another function definition") {
      val code =
        """
           val factorial = (x) => { if (x == 0) { return 1; } else { return x * factorial(x - 1); } };

           return factorial(10);
        """.stripMargin

      assert(parseAndRunBlock(code) == BlInt(3628800))
    }
  }


}
