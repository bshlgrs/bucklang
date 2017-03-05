package parser

/**
  * Created by buck on 3/4/17.
  */

import fastparse.WhitespaceApi
import interpreter._

object Parser {
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import White._
  import fastparse.noApi._

  lazy val name: P[String] = P((CharIn('a' to 'z', 'A' to 'Z').repX(1) ~ ("!" | "?").?).!)
  val number: P[BlInt] = P( CharIn('0'to'9').rep(1).!.map({case (x: String) => BlInt(x.toInt)}))
  lazy val numberExpr: P[BlExpression] = number.map((x) => LiteralExpr(x))

  lazy val parensExpr: P[BlExpression] = P("(" ~ expr ~ ")")
  lazy val arrayLiteral: P[BlExpression] = P("[" ~ functionArgument.rep(sep = ",") ~ "]")
    .map({ case (x: Seq[BlFunctionArgument]) => FunctionCallExpr(LiteralExpr(BlListClass), x.toList)})

  lazy val nameExpr: P[NameExpr] = name.map((x) => NameExpr(x))

  lazy val thisExpr: P[BlExpression] = P("this").map((_) => ThisExpr)

  lazy val exprLevel1: P[BlExpression] = P(thisExpr | numberExpr | nameExpr | parensExpr | arrayLiteral)

  lazy val exprLevel2: P[BlExpression] = P(
    exprLevel1 ~ (
      ("." ~ name ~ ("(" ~ functionArgument.rep(sep = ",") ~ ")")) |
        ("." ~ name) |
        ("(" ~ functionArgument.rep(sep = ",") ~ ")")
      ).rep ).map({
    case (callee: BlExpression, args: Seq[Object]) => args.foldLeft(callee) ({
      case (callee2: BlExpression, (name: String, args: Seq[BlFunctionArgument])) =>
        MethodCallExpr(callee2, name, args.toList)
      case (callee2: BlExpression, (name: String)) =>
        FieldAccessExpr(callee2, name)
      case (callee2: BlExpression, (args: Seq[BlFunctionArgument])) =>
        FunctionCallExpr(callee2, args.toList)
    })
  })

  lazy val exprLevel4: P[BlExpression] = P( exprLevel2 ~ (CharIn("*/").! ~/ exprLevel2).rep ).map({
    case (exp: BlExpression, rest: Seq[(String, BlExpression)]) => rest.foldLeft(exp)({
      case (l, (char, r)) => MethodCallExpr(l, char, List(LiteralFunctionArgument(r)))
    })
  })

  lazy val exprLevel5: P[BlExpression] = P( exprLevel4 ~ (CharIn("+-").! ~/ exprLevel4).rep ).map({
    case (exp: BlExpression, rest: Seq[(String, BlExpression)]) => rest.foldLeft(exp)({
      case (l, (char, r)) => MethodCallExpr(l, char, List(LiteralFunctionArgument(r)))
    })
  })


  lazy val functionArgument: P[BlFunctionArgument] =
    P(expr.map(LiteralFunctionArgument) | ("..." ~ expr).map({ case (x) => SplatFunctionArgument(x)}))


  lazy val expr: P[BlExpression] = exprLevel5

  lazy val nakedExpr: P[BlExpression] = expr ~ End

  def main(args: Array[String]): Unit = {
    print(nakedExpr.parse("foo(bar)(baz)").get.value)


  }
}
