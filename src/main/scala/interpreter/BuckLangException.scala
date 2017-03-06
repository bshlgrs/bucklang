package interpreter

/**
  * Created by buck on 3/4/17.
  */
class BuckLangException(s: String) extends RuntimeException(s) {

}

class BuckLangValueError(s: String, v: BlValue) extends BuckLangException(s ++ "\n\n" ++ v.toString)
