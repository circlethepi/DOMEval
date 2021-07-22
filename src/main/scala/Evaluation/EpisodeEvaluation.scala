package Evaluation

import DistroManager.Card

import scala.collection.mutable.ListBuffer

case class EpisodeEvaluation(evaluations : List[GameEvaluation]) extends Distribution {

  val bigDistribution : (Double, Double) = {

    val valsBuff = new ListBuffer[Double]

    for (i <- evaluations) {
      valsBuff+= i.collect_all_values()
      }

    val bigMu = mean(valsBuff.toList)
    val bigSig = stdev(valsBuff.toList)

    (bigMu, bigSig)
    }

  val cardDistributions : List[(Card, Double, Double)] = {


    for (i <- evaluations) {

    }

    List()

  }




}

