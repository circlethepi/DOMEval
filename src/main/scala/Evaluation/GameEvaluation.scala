package Evaluation

import DistroManager.Card

import scala.collection.mutable.ListBuffer

case class GameEvaluation(evals : List[(Card,Double)]) {

  def collect_all_values() : List[Double] = {
    val valsBuff = new ListBuffer[Double]

    for (i <- evals) {
      valsBuff += i._2
    }

    valsBuff.toList
  }

}
