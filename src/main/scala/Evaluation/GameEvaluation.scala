package Evaluation

import DistroManager.Card

import scala.collection.mutable.ListBuffer

case class GameEvaluation(val evals : List[(Card,Double)]) {

  def collect_all_values() : List[Double] = {
    val valsBuff = new ListBuffer[Double]

    for (i <- evals) {
      valsBuff += i._2
    }

    valsBuff.toList
  }

  def get_val_of(card : String) : Option[Double] = {
    for(c <- evals) {
      if(card.compare(c._1.cardname) == 0) {
        return Some(c._2)
      }
    }
    None
  }

}
