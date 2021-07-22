package Evaluation

import DistroManager.Card

import scala.collection.mutable.ListBuffer

case class EpisodeEvaluation(kingdom : Kingdom, evaluations : List[GameEvaluation]) extends Distribution {

  //big distribution of all evals for all cards in one place
  //for a SINGLE EPISODE
  val bigDistribution : (Double, Double) = {

    val valsBuff = new ListBuffer[Double]

    for (i <- evaluations) {
      valsBuff += i.collect_all_values()
    }

    val bigMu = mean(valsBuff.toList)
    val bigSig = stdev(valsBuff.toList)

    (bigMu, bigSig)
  }

  val cardDistributions : List[(Card, Double, Double)] = {

    //individual "little" card distributions
    def cardDistributions(): List[Card] = {
      for (c <- kingdom.cards) {
        for (e <- evaluations) {
          c.add_to_sample(e.get_val_of(c.cardname).get)
        }
      }

      kingdom.cards //list of cards holding their episode distributions
    }

    override def toString: String = {
      var str = ""
      for (c <- kingdom.cards) {
        str += c.cardname + " " + c.mu + " " + c.sigma + ","
      }
      str = str.substring(0, str.length - 1)
      str
    }


  }
}


