package Critic

import DistroManager._
import Evaluation.EpisodeEvaluation

import scala.collection.mutable.ListBuffer

case class Critique(hypotheses : HypothesisSet, evaluation : EpisodeEvaluation) {

  val significanceLevel : Double = {
    //% are the probabilities that the mean differs by this amount purely by chance
    //1.65 //for 10%
    //1.96 //for 5%
    2.58 //for 1%
    //
  }

  val hypoDistros : List[(Card, Double, Double)] = {
    val hypoParamsBuff = new ListBuffer[(Card, Double, Double)]

    for (h <- hypotheses.hypotheses) {
      val buffAdd = (h.card, h.mu, h.sig)
      hypoParamsBuff += buffAdd
    }

    hypoParamsBuff.toList
  }

  val epDistros : List[(Card,Double,Double)] = {
    val epParamsBuff = new ListBuffer[(Card, Double, Double)]

    for (card <- evaluation.cardDistributions) {
      val buffAdd = (card.cardname, card.mu, card.sigma)
      epParamsBuff += buffAdd
    }

    epParamsBuff.toList
  }




}
