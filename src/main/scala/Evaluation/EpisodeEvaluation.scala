package Evaluation

import DistroManager.Card

import scala.collection.mutable.ListBuffer

case class EpisodeEvaluation(val kingdom : Kingdom, val evaluations : List[GameEvaluation]) extends Distribution {

  //big distribution of all evals for all cards in one place
  //for a SINGLE EPISODE
  val bigDistribution : (Double, Double) = {
    for (i <- evaluations) {
      add_to_sample( i.collect_all_values() )
    }

    (mean(sample), stdev(sample))
  }

  //individual "little" card distributions
  val cardDistributions : List[Card] = {
    for (c <- kingdom.cards) {
      for (e <- evaluations) {
        c.add_to_sample(e.get_val_of(c.cardname).get)
      }

    }
    kingdom.cards
  }

  override def toString : String = {
    var str = ""
    for (c <- kingdom.cards) {
      str += c.cardname + " " + c.mean(c.sample) + " " + c.stdev(c.sample)  + ":"
    }
    str = str.substring(0, str.length - 1)
    str
  }

}


