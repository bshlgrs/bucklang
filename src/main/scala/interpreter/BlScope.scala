package interpreter

import scala.collection.mutable

/**
  * Created by buck on 3/4/17.
  */
class BlScope(thisVal: Option[BlObject],
              mbParent: Option[BlScope],
              vars: mutable.Map[String, BlValue] = mutable.Map(),
              vals: mutable.Map[String, BlValue] = mutable.Map()) {


  def declareVals(map: Map[String, BlValue]) = {
    for {
      (name, value) <- map
    } declareVal(name, value)
  }

  def get(name: String): Option[BlValue] = {
    vars.get(name).orElse(vals.get(name)).orElse(mbParent.flatMap((p) => p.get(name)))
  }

  def getThis: Option[BlObject] = thisVal.orElse(mbParent.flatMap(_.getThis))

  def set(name: String, newVal: BlValue): Unit = {
    if (vars.contains(name)) {
      vars(name) = newVal
    } else if (vals.contains(name)) {
      throw new RuntimeException(s"tried to set $name, but it is a val")
    } else {
      mbParent match {
        case None => throw new RuntimeException(s"tried to set $name, but it is not defined")
        case Some(parent) => parent.set(name, newVal)
      }
    }
  }

  def varsPlusVals: Map[String, BlValue] = vars.toMap ++ vals

  def declareVar(name: String, newVar: BlValue): Unit = {
    if (varsPlusVals.contains(name)) {
      throw new RuntimeException(s"name $name is already in use")
    } else {
      vars(name) = newVar
    }
  }

  def declareVal(name: String, newVal: BlValue): Unit = {
    if (varsPlusVals.contains(name)) {
      throw new RuntimeException(s"name $name is already in use")
    } else {
      vals(name) = newVal
    }
  }

}

object BlScope {
}
