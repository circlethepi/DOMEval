package Critic

import DistroManager._
import Evaluation._

import scala.collection.mutable.ListBuffer

case class Critique(hypotheses : HypothesisSet, evaluation : EpisodeEvaluation) {

  val significanceLevel : Double = {
    //% are the probabilities that the mean differs from the hypothesis by this #SDs purely by chance
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
      val buffAdd = (card, card.mean(card.sample), card.stdev(card.sample))
      epParamsBuff += buffAdd
    }

    epParamsBuff.toList
  }


  def get_differences() : List[(Card, Double)] = {

    val distBuff = ListBuffer[(Card, Double)]()

    for (distrEp <- epDistros) {
      for (distrHyp <- hypoDistros) {
        if (distrEp._1.cardname.compare(distrHyp._1.cardname) == 0) { //episode - hypothesis => negative means episode < hypothesis

          val card : Card = distrEp._1
          val muEp = distrEp._2
          val muHyp = distrHyp._2
          val sigHyp = distrHyp._3

          //we calculate #SDs from hyp mean using hyp distr. info since hypothetically:
          //the episode samples should come from the hypothesis distribution
          //a wildly different result indicates either some sort of interaction or not enough about the distribution is known yet

          val numSDs = (muEp-muHyp) / sigHyp

          val add = (card, numSDs)

          distBuff += add

        }
      }
    }

    distBuff.toList
  }


  def distance_flag(distances : List[(String, Double)]) : List[String] = {
    val flagBuff = new ListBuffer[String]()
    val flagSDCutOff = significanceLevel

    for (card <- distances) {
      if (card._2 >= flagSDCutOff) {

        flagBuff += card._1

      }
    }
    flagBuff.toList
  }

}
