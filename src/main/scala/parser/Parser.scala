package parser

/**
  * Created by buck on 3/4/17.
  */

import fastparse.WhitespaceApi
import fastparse.all._
import interpreter._

object Parser {
  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V){
    def apply(t: T) = f(t)
    override def toString() = name

  }

  lazy val myWhitespace = CharIn(" ", "\n").rep

  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(myWhitespace)
  }
  import White._
  import fastparse.noApi._

  lazy val name: P[String] = P(
    CharIn('a' to 'z', 'A' to 'Z', Seq('_')) ~ CharIn('a' to 'z', 'A' to 'Z', Seq('_'), '0' to '9').rep ~ ("!" | "?").?).!
//  lazy val name: P[String] = P(CharIn('a' to 'z', 'A' to 'Z') ~ CharIn('a' to 'z', 'A' to 'Z', Seq('_')).repX(1) ~ ("!" | "?").?).!

  val number: P[BlInt] = P( CharIn('0'to'9').rep(1).!.map({case (x: String) => BlInt(x.toInt)}))
  lazy val numberExpr: P[BlExpression] = number.map((x) => LiteralExpr(x))

  lazy val parensExpr: P[BlExpression] = P("(" ~ expr ~ ")")
  lazy val arrayLiteral: P[BlExpression] = P("[" ~ functionArgument.rep(sep = ",") ~ "]")
    .map({ case (x: Seq[BlFunctionArgument]) => FunctionCallExpr(LiteralExpr(BlListClass), x.toList)})

  lazy val objectLiteral: P[BlExpression] =
    P("{" ~ objectLiteralArgument.rep(sep = ",") ~ "}")
    .map({ case (x: Seq[Either[(BlExpression, BlExpression), BlExpression]]) => {
      val emptyMap = LiteralExpr(BlMap(Map()))

      x.toList.foldLeft[BlExpression](emptyMap)({
        case (map: BlExpression, Left((fieldName: BlExpression, value: BlExpression))) =>
          MethodCallExpr(map, "set", List(LiteralFunctionArgument(fieldName), LiteralFunctionArgument(value))) : MethodCallExpr
        case (map: BlExpression, Right(map2: BlExpression)) =>
          MethodCallExpr(map, "++", List(LiteralFunctionArgument(map2)))
      })
    }})

  lazy val objectLiteralArgument: P[Either[(BlExpression, BlExpression), BlExpression]] =
    P(
      ("[" ~ name ~ "]" ~ ":" ~ expr).map({ case (s: String, e: BlExpression) => Left(NameExpr(s) -> e) }) |
      (name ~ ":" ~ expr).map({ case (s: String, e: BlExpression) => Left(LiteralExpr(BlString(s)) -> e) }) |
      (expr ~ ":" ~ expr).map({ case (s: BlExpression, e: BlExpression) => Left(s -> e) }) |
      ("..." ~ expr).map({ case (e: BlExpression) => Right(e) })
    )



  val Whitespace = NamedFunction(" \r\n".contains(_: Char), "Whitespace")

  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

  val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
  val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )
  val strChars = P( CharsWhile(StringChars) )
  val stringLiteral =
    P("\"" ~/ (strChars | escape).rep.! ~ "\"").map((x) => LiteralExpr(BlString(x)))

  lazy val nameExpr: P[NameExpr] = name.map((x) => NameExpr(x))

  lazy val thisExpr: P[BlExpression] = P("this").map((_) => ThisExpr)

  lazy val exprLevel1: P[BlExpression] = P(thisExpr | numberExpr | nameExpr | parensExpr | arrayLiteral | objectLiteral | stringLiteral)

  lazy val exprLevel2: P[BlExpression] = P(
    CharIn("!", "-").!.rep ~ exprLevel1 ~ (
      ("." ~ name ~ ("(" ~ functionArgument.rep(sep = ",") ~ ")")) |
        ("." ~ name) |
        ("(" ~ functionArgument.rep(sep = ",") ~ ")")
      ).rep ).map({
    case (unaryOps: Seq[String], callee: BlExpression, args: Seq[Object]) => {
      val mainExpr = args.foldLeft(callee) ({
        case (callee2: BlExpression, (name: String, args: Seq[BlFunctionArgument])) =>
          MethodCallExpr(callee2, name, args.toList)
        case (callee2: BlExpression, (name: String)) =>
          FieldAccessExpr(callee2, name)
        case (callee2: BlExpression, (args: Seq[BlFunctionArgument])) =>
          FunctionCallExpr(callee2, args.toList)
      })

      unaryOps.foldLeft(mainExpr) ({
        case (e: BlExpression, "-") => MethodCallExpr(e, "__negate__", List())
        case (e: BlExpression, "!") => MethodCallExpr(e, "__not__", List())
      })
    }
  })

  lazy val exprLevel4: P[BlExpression] = P( exprLevel2 ~ (CharIn("*/").! ~/ exprLevel2).rep ).map({
    case (exp: BlExpression, rest: Seq[(String, BlExpression)]) => rest.foldLeft(exp)({
      case (l, (char, r)) => MethodCallExpr(l, char, List(LiteralFunctionArgument(r)))
    })
  })

  lazy val exprLevel5: P[BlExpression] = P( exprLevel4 ~ (StringIn("+", "-", "++").! ~/ exprLevel4).rep ).map({
    case (exp: BlExpression, rest: Seq[(String, BlExpression)]) => rest.foldLeft(exp)({
      case (l, (char, r)) => MethodCallExpr(l, char, List(LiteralFunctionArgument(r)))
    })
  })

  lazy val exprLevel6: P[BlExpression] = P( exprLevel5 ~ (("==".! | "!=".! | ">".! | ">=".! | "<".! | "<=") ~ exprLevel5).?).map({
    case (x: BlExpression, None) => x
    case (lhs: BlExpression, Some((string: String, rhs: BlExpression))) =>
      MethodCallExpr(lhs, string, List(LiteralFunctionArgument(rhs)))
  })

  lazy val exprLevel7: P[BlExpression] = P( exprLevel6 ~ ("&&" ~ exprLevel6).?).map({
    case (x: BlExpression, None) => x
    case (lhs: BlExpression, Some(rhs: BlExpression)) =>
      MethodCallExpr(lhs, "&&", List(LiteralFunctionArgument(FunctionDefinitionExpr.lazyWrap(rhs))))
  })

  lazy val exprLevel8: P[BlExpression] = P( exprLevel7 ~ ("||" ~ exprLevel7).?).map({
    case (x: BlExpression, None) => x
    case (lhs: BlExpression, Some(rhs: BlExpression)) =>
      MethodCallExpr(lhs, "||", List(LiteralFunctionArgument(FunctionDefinitionExpr.lazyWrap(rhs))))
  })

  lazy val functionArgument: P[BlFunctionArgument] =
    P(expr.map(LiteralFunctionArgument) | ("..." ~ expr).map({ case (x) => SplatFunctionArgument(x)}))


  lazy val functionDefinitionExpr: P[BlExpression] = P(
    "(" ~ arrayDestructurePatternInner ~ ")" ~ "=>" ~ (exprLevel9 | "{" ~ blockOfStatements ~ "}")).map({
    case (a: ArrayDestructurePattern, expr: BlExpression) => FunctionDefinitionExpr(a, Map(), List(ReturnStatement(Some(expr))))
    case (a: ArrayDestructurePattern, block: List[BlStatement]) => FunctionDefinitionExpr(a, Map(), block)
  })

  lazy val exprLevel9: P[BlExpression] = P(functionDefinitionExpr | exprLevel8)

  lazy val expr: P[BlExpression] = exprLevel9

  lazy val nakedExpr: P[BlExpression] = naked(expr)

  lazy val simplePattern: P[BlPattern] = P(name).map(SingleVariablePattern)

  lazy val arrayDestructurePattern = P("[" ~ arrayDestructurePatternInner ~ "]")
  lazy val arrayDestructurePatternInner: P[BlPattern] = P(pattern.rep(sep=",") ~ ("," ~ "..." ~ name ~ ("," ~ pattern).rep).?).map({
    case (initials, splat) => ArrayDestructurePattern(initials.toList, splat.map({
      case (splatName: String, lastArgs: Seq[BlPattern]) => splatName -> lastArgs.toList
    }))
  })
  lazy val objectDestructurePattern: P[BlPattern] = P(
    "{" ~ name.rep(sep=",") ~ ("," ~ "..." ~ name).? ~ "}").map({
    case (initials, splatName) => ObjectDestructurePattern(initials.toSet, splatName)
  })

  lazy val pattern: P[BlPattern] = P(
    simplePattern
      | arrayDestructurePattern
      | objectDestructurePattern
      | P("_").map((_) => UnderscorePattern ))

  lazy val valDeclStatement: P[BlStatement] = P("val" ~ pattern ~ "=" ~ expr ~ ";").map({
    case (p: BlPattern, e: BlExpression) => ValDeclStatement(p, e)
  })

  lazy val varDeclStatement: P[BlStatement] = P("var" ~ name ~ "=" ~ expr ~ ";").map({
    case (name: String, e: BlExpression) => VarDeclStatement(name, e)
  })

  lazy val varSetStatement: P[BlStatement] = P(name ~ "=" ~ expr ~ ";").map({
    case (name: String, e: BlExpression) => VarSetStatement(name, e)
  })

  lazy val fieldSetStatement: P[BlStatement] = P(exprLevel1 ~ "." ~ name ~ "=" ~ expr ~ ";").map({
    case (lhs: BlExpression, name: String, e: BlExpression) => FieldSetStatement(lhs, name, e)
  })

  lazy val returnStatement: P[BlStatement] = P("return" ~ expr ~ ";").map({
    case (e: BlExpression) => ReturnStatement(Some(e))
  })

  lazy val whileStatement: P[BlStatement] = P("while" ~ "(" ~ expr ~ ")" ~ "{" ~ blockOfStatements ~ "}").map({
    case (cond: BlExpression, body: List[BlStatement]) => WhileStatement(cond, body)
  })

  lazy val ifStatement: P[BlStatement] = P("if" ~ "(" ~ expr ~ ")" ~ "{" ~ blockOfStatements ~ "}" ~
    ("else" ~ "{" ~ blockOfStatements ~ "}").?).map({
    case (cond: BlExpression, body: List[BlStatement], elseCase: Option[List[BlStatement]]) =>
      IfStatement(cond, body, elseCase.getOrElse(Nil))
  })


  lazy val blockOfStatements: P[List[BlStatement]] = P(stmt.rep).map(_.toList)

  lazy val nakedBlockOfStatements = naked(blockOfStatements)

  lazy val stmt: P[BlStatement] = P(
      valDeclStatement
      | varDeclStatement
      | varSetStatement
      | fieldSetStatement
      | returnStatement
      | (expr ~ ";").map((e) => ExprStatement(e))
      | whileStatement
      | ifStatement
    )

  lazy val nakedStmt = naked(stmt)

  lazy val fieldInClass: P[(Boolean, String, BlExpression)] = {
    val functionField = P(name ~ "(" ~ arrayDestructurePatternInner ~ ")" ~ "{" ~ blockOfStatements ~ "}").map({
      case (name: String, args: ArrayDestructurePattern, statements: List[BlStatement]) =>
        (false, name, FunctionDefinitionExpr(args, Map(), statements))
    })

    val staticField = P(name ~ "=" ~ expr).map({ case (name: String, expr: BlExpression) => (false, name, expr)})

    P(
      functionField | staticField
    )
  }

//  lazy val classDef: P[BlStatement] = P(
//    "class" ~ name ~
//      "(" ~ name.rep(sep=",") ~ ")" ~ "{" ~ fieldInClass.rep ~ "}").map({
//    case (name: String, args: ArrayDestructurePattern, fields: Seq[(Boolean, String, BlExpression)]) => {
//      ClassDeclStatement(SingleVariablePattern(name), )
//    }
//  })

  def naked[A](p: P[A]): P[A] = P(myWhitespace ~ p ~ myWhitespace ~ End)

  def main(args: Array[String]): Unit = {
    println(naked(objectLiteral).parse("{a: 1}").get.value)
  }
}
