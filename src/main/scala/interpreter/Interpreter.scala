package interpreter


/**
  * Created by buck on 3/4/17.
  */
class Interpreter {

}

object Interpreter {
  def main(args: Array[String]): Unit = {
    println("hello world!")
  }

  def run(module: BlModule): Unit = {
    module.defs.get("main")

  }
}
