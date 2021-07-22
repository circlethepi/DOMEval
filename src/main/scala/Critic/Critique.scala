package Critic

import DistroManager.Hypothesis
import Evaluation.EpisodeEvaluation

case class Critique(hypotheses : HypothesisSet, evaluation : EpisodeEvaluation) {

//  val significanceLevel : Double =

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
