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
        """val x = range(10);
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
        """ val factorial = (x) => { if (x == 0) { return 1; } else { return x * factorial(x - 1); } };

           return factorial(10);
        """.stripMargin

      assert(parseAndRunBlock(code) == BlInt(3628800))
    }

    it("can use objects") {
      val code =
        """
          |val object = { a: 1, b: 2, c: 5 };
          |return object.get("a") + object.get("b") + object.get("c");
        """.stripMargin

      assert(parseAndRunBlock(code) == BlInt(8))
    }

    describe("Arrays") {
      it("can use arrays") {
        val code =
          """
            |val array = [5, 2, 3].set(1, 5);
            |return array.get(0) + array.get(1) + array.get(2) - array.sum();
          """.stripMargin

        assert(parseAndRunBlock(code) == BlInt(0))
      }

      it("can use `map`") {
        assert(parseAndRunBlock("return [5, 2, 3].map((x) => x * 2).sum();") == BlInt(20))
      }
    }

  }


}
