package utils

import scala.util.{Success, Try}

/**
  * Created by buck on 3/4/17.
  */
object Utils {
  def listOfTriesIntoTryOfList[A](list:  List[Try[A]]): Try[List[A]] = {
    list.find(_.isFailure) match {
      case None => Success(list.map(_.get))
      case Some(failure) => failure.map((x) => List(x)) // the code inside the map never actually happens
    }
  }
}
