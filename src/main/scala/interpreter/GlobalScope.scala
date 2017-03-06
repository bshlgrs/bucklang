package interpreter

/**
  * Created by buck on 3/5/17.
  */
object GlobalScope extends BlScope(None, None) {
  declareVal("range", range)
  declareVal("List", BlListClass)

  object range extends BlCallable {
    def call(params: List[BlValue]): BlValue = params match {
      case List(x) => x match {
        case BlInt(r) => BlList(List.range(0, r).map(BlInt))
        case _ => throw new BuckLangException(s"$x is not an int")
      }
      case List(x, y) => (x, y) match {
        case (BlInt(l), BlInt(r)) => BlList(List.range(l, r).map(BlInt))
        case _ => throw new BuckLangException(s"Arguments to range were not both ints")
      }
    }
  }
}

