package interpreter

/**
  * Created by buck on 3/4/17.
  */


import scala.collection.mutable

abstract class BlValue {
  def truthy: Boolean

  def handleMethodCall(name: String,
                       flattenedArgs: List[BlValue],
                       scope: BlScope): BlValue = {

    getMethod(name) match {
      case None => throw new RuntimeException(s"No method $name on object $this")
      case Some((arity, method)) => {
        if (arity != flattenedArgs.length) {
          throw new RuntimeException(s"Called the method $name on $this with ${flattenedArgs.length} args; needs $arity")
        } else {
          method(flattenedArgs)
        }
      }
    }
  }

  def getMethod(name: String): Option[InbuiltMethod] = {
    universalMethods.get(name).orElse(inbuiltMethods.get(name))
  }

  type InbuiltMethod = (Int, List[BlValue] => BlValue)


  def inbuiltMethods: Map[String, InbuiltMethod]

  val universalMethods = Map[String, InbuiltMethod](
    "&&" -> (1 -> {case List(x: BlValue) => this.and(x) }),
    "||" -> (1 -> {case List(x: BlValue) => this.or(x) }),
    "==" -> (1 -> {case List(x: BlValue) => BlBool(this == x) }),
    "!=" -> (1 -> {case List(x: BlValue) => BlBool(this != x) }),
    "__not__" -> (0 -> {case List() => BlBool(!this.truthy) })
  )

  def and(other: BlValue): BlValue = {
    if (truthy) {
      other.asInstanceOf[BlCallable].call(Nil)
    } else {
      this
    }
  }

  def or(other: BlValue): BlValue = {
    if (truthy) {
      other
    } else {
      other.asInstanceOf[BlCallable].call(Nil)
    }
  }

  def getField(field: String): BlValue = {
    this.objectFields.get(field) match {
      case Some(f) => f()
      case None => throw new RuntimeException(s"Object $this has no field $field")
    }
  }

  def setField(field: String, value: BlValue): Unit = throw new BuckLangException(s"can't set fields on $this (tried to set $field)")

  def objectFields: Map[String, () => BlValue] = Map()
}

case class BlInt(int: Int) extends BlValue {
  def truthy = true

  override def toString: String = int.toString

  val inbuiltMethods = Map[String, InbuiltMethod](
    "+" -> (1 -> {case List(x: BlValue) => this.numericalOperate(x, (a: Int, b: Int) => a + b) }),
    "-" -> (1 -> {case List(x: BlValue) => this.numericalOperate(x, (a: Int, b: Int) => a - b) }),
    "/" -> (1 -> {case List(x: BlValue) => this.numericalOperate(x, (a: Int, b: Int) => a / b) }),
    "*" -> (1 -> {case List(x: BlValue) => this.numericalOperate(x, (a: Int, b: Int) => a * b) }),
    ">" -> (1 -> {case List(x: BlValue) => this.comparisonOperate(x, (a: Int, b: Int) => a > b) }),
    "<" -> (1 -> {case List(x: BlValue) => this.comparisonOperate(x, (a: Int, b: Int) => a < b) }),
    ">=" -> (1 -> {case List(x: BlValue) => this.comparisonOperate(x, (a: Int, b: Int) => a >= b) }),
    "<=" -> (1 -> {case List(x: BlValue) => this.comparisonOperate(x, (a: Int, b: Int) => a <= b) }),
    "__negate__" -> (0 -> {case List() => BlInt(-int) })
  )

  def numericalOperate(other: BlValue, f: (Int, Int) => Int): BlValue = other match {
    case BlInt(otherInt) => BlInt(f(this.int, otherInt))
    case _ => throw new BuckLangValueError("can't numerical operate", other)
  }

  def comparisonOperate(other: BlValue, f: (Int, Int) => Boolean): BlValue = other match {
    case BlInt(otherInt) => BlBool(f(this.int, otherInt))
  }
}

case class BlString(string: String) extends BlValue {
  def truthy = true

  val inbuiltMethods = Map[String, InbuiltMethod](
    "reverse" -> (0 -> ((_) => BlString(string.reverse)))
  )

  override val objectFields = Map("length" -> (() => BlInt(string.length)))
}

case class BlBool(boolean: Boolean) extends BlValue {
  def truthy = boolean

  val inbuiltMethods = Map[String, InbuiltMethod](

  )

}

case object BlNull extends BlValue {
  def truthy = false

  val inbuiltMethods: Map[String, (Int, (List[BlValue]) => BlValue)] = Map()
}

abstract class BlCallable extends BlValue {
  def truthy = true

  val inbuiltMethods: Map[String, (Int, (List[BlValue]) => BlValue)] = Map()

  def call(parameters: List[BlValue]): BlValue
}

case class BlFunctionDefinition(expr: FunctionDefinitionExpr,
                                scope: BlScope)
  extends BlCallable {

  def call(parameters: List[BlValue]): BlValue = {
    val newScope = new BlScope(None, Some(scope))
    val destructuredArgs = expr.defaultVals ++ expr.args.destructure(BlList(parameters)).get
    newScope.declareVals(destructuredArgs)
    BlStatement.evalBlock(newScope, expr.body).getOrElse(BlNull)
  }
}

class BlObject(blClass: BlClass, initialFields: Map[String, BlValue]) extends BlValue {
  val fields: mutable.Map[String, BlValue] = mutable.Map() ++ initialFields
  def truthy = true

  override def getMethod(name: String): Option[InbuiltMethod] = blClass.getMethod(name)

  val inbuiltMethods: Map[String, (Int, (List[BlValue]) => BlValue)] = Map()

  override val objectFields: Map[String, () => BlValue] = this.fields.mapValues((x) => () => x).toMap ++ Map("class" -> (() => this.blClass))

  override def setField(field: String, value: BlValue): Unit = {
    fields(field) = value
  }
}

case class BlMap(map: Map[BlValue, BlValue]) extends BlValue {
  def truthy = true

  val inbuiltMethods: Map[String, (Int, (List[BlValue]) => BlValue)] = Map(
    "get" -> (1 -> {case (List(x)) => map.getOrElse(x, BlNull)}),
    "set" -> (2 -> {case (List(k: BlValue, v: BlValue)) => {
      if (map.getOrElse(k, BlNull) == v)
        this
      else
        BlMap(map.updated(k, v))
    }}),
    "++" -> (1 -> {case (List(v: BlMap)) => BlMap(map ++ v.map)}),
    "contains" -> (1 -> {case (List(k: BlObject)) => BlBool(map.contains(k))})
  )
}

case class BlList(list: List[BlValue]) extends BlValue {
  def truthy = true

  val inbuiltMethods: Map[String, (Int, (List[BlValue]) => BlValue)] = Map(
    "reverse" -> (0 -> ((_) => BlList(list.reverse))),
    "get" -> (1 -> {case (List(BlInt(i))) => {
      if (i >= 0 && i < list.length)
        list(i)
      else
        throw new BuckLangValueError("Array access out of bounds", BlInt(i))
    }}),
    "set" -> (2 -> {case (List(BlInt(i), v: BlValue)) => {
      if (i >= 0 && i < list.length)
        BlList(list.updated(i, v))
      else
        throw new BuckLangValueError("Array access out of bounds", BlInt(i))
    }}),
    "append" -> (1 -> {case (List(v: BlValue)) => BlList(list :+ v)}),
    "prepend" -> (1 -> {case (List(v: BlValue)) => BlList(v +: list)}),
    "++" -> (1 -> {case (List(v: BlList)) => BlList(list ++ v.list)}),
    "map" -> (1 -> {case (List(v: BlCallable)) => BlList(list.map((x) => v.call(List(x))))}),
    "filter" -> (1 -> {case (List(v: BlCallable)) => BlList(list.filter((x) => v.call(List(x)).truthy))})
//    "reduce" -> (1 -> {case (List(v: BlCallable)) => {
//
//    }})
  )

  override val objectFields = Map("size" -> (() => BlInt(list.size)))
}

case class BlClass(name: String,
                   params: List[String],
                   defaultVals: Map[String, BlValue],
                   decls: Map[String, BlValue],
                   classDecls: Map[String, BlValue],
                   parent: Option[BlClass]
                  ) extends BlValue {
  override def toString: String = s"Class<$name>"

  def truthy: Boolean = true

  val inbuiltMethods: Map[String, (Int, (List[BlValue]) => BlValue)] = Map()

  def construct(args: List[BlValue]): BlValue = {
    val passedParameters = args.zip(params).map({ case (value, paramName) => paramName -> value}).toMap
    val constructionParameters = defaultVals ++ passedParameters

    if (constructionParameters.size < params.length) {
      throw new BuckLangException(s"Not enough arguments passed to constructor for $name")
    }

    new BlObject(this, constructionParameters)
  }
}

object BlListClass extends BlClass("List", List(), Map(), Map(), Map(), None) {
  override def construct(args: List[BlValue]): BlValue = BlList(args)
}

object BlMapClass extends BlClass("Map", List(), Map(), Map(), Map(), None) {
  override def construct(args: List[BlValue]): BlValue = {
    if (args.forall(_.isInstanceOf[BlMap])) {
      BlMap(args.map(_.asInstanceOf[BlMap].map).reduce(_ ++ _))
    } else {
      throw new BuckLangValueError("arg is not a map", args.find(!_.isInstanceOf[BlMap]).get)
    }
  }
}
