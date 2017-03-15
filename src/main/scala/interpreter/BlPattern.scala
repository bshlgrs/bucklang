package interpreter

import parser.Parser
import utils.Utils

import scala.util.{Failure, Success, Try}
/**
  * Created by buck on 3/4/17.
  */
abstract class BlPattern {
  def destructure(value: BlValue): Try[Map[String, BlValue]] = this match {
    case SingleVariablePattern(name) => Success(Map(name -> value))
    case ArrayDestructurePattern(list, None) => value match {
      case BlList(paramsList) if paramsList.length == list.length => {
        Utils.listOfTriesIntoTryOfList(list.zip(paramsList)
          .map({ case (pat: BlPattern, v: BlValue) => pat.destructure(v) })).map((x) => x.reduce(_ ++ _))
      }
      case BlList(paramsList) if paramsList.length != list.length => Failure(new BuckLangException("Destructured list is wrong length"))
      case x => Failure(new RuntimeException(s"Trying to destructure $x into list"))
    }
    case ArrayDestructurePattern(initialParamsList, Some((splatName, endPatterns))) => value match {
      case BlList(paramsList) => {
        if (initialParamsList.length + endPatterns.length > paramsList.length) {
          Failure(new BuckLangException("Too few elements in array to fit to destructure"))
        } else {
          val matchedThings: List[(BlPattern, BlValue)] =
            initialParamsList.zip(paramsList.take(initialParamsList.length)) ++
              endPatterns.zip(paramsList.takeRight(endPatterns.length))

          val matches: List[Try[Map[String, BlValue]]] = matchedThings.map({ case (p: BlPattern, v: BlValue) => p.destructure(v)})

          Utils.listOfTriesIntoTryOfList(matches).map(_.reduce(_ ++ _))
        }
      }
    }
    case UnderscorePattern => Success(Map())
  }
}

case class SingleVariablePattern(name: String) extends BlPattern
case class ArrayDestructurePattern(initialPatterns: List[BlPattern],
                                   splat: Option[(String, List[BlPattern])]
                                  ) extends BlPattern {
  def destructureAllowingExtras(list: List[BlValue]): Try[Map[String, BlValue]] = {
    if (list.length < initialPatterns.length + splat.map(_._2.length).getOrElse(0)) {
      Failure(new BuckLangException("Too few elements in array to fit to destructure"))
    } else {
      splat match {
        case None => Utils.listOfTriesIntoTryOfList(list.zip(initialPatterns)
          .map({ case (v: BlValue, pat: BlPattern) => pat.destructure(v) })).map((x) => x.reduce(_ ++ _))
        case _ => ???
      }
    }
  }
}

object ArrayDestructurePattern {
  def simple(vars: String*) = ArrayDestructurePattern(vars.toList.map(SingleVariablePattern), None)
}

case class ObjectDestructurePattern(names: Set[String], nameForRest: Option[String])
  extends BlPattern

case object UnderscorePattern extends BlPattern
