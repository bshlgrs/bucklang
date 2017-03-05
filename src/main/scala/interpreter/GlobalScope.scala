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
        case x => throw new BuckLangException(s"$x is not an int")
      }
    }
  }
}

