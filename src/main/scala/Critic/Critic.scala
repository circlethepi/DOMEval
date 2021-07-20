package Critic

import DistroManager._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

object Critic {
  val cards: List[String] = {
    Source.fromFile("cardList.txt").getLines.toList
  }

  /**
   * Author: Merrick Ohata
   * Date: July 2021
   *
   * CRITIC for DOMINION
  *
  * */

  def read_hypotheses() : Unit = {
   // val h = Hypothesis(Card("test"))

  }

  /**
   *
   * @param lognumStart starting logNumber
   *
   * from a starting logNumber, collects all the evaluations of each card into a list of string, List(values)
   *
   */

  def get_episode_points(lognumStart : Int) : List[(String, List[Double])] = {

    val distroBuff = new ListBuffer[(String, List[Double])]

    for (cardname <- cards) {         //for each card
      val evals: List[Double] = {     //get the list of values

        val evalsBuff = new ListBuffer[Double]
        for (i <- 0 until 30) {
          val lognumber = lognumStart + i
          val filename = "evaluations/cardEvaluations" + lognumber.toString + ".csv"
          val epNumsFile = io.Source.fromFile(filename)

          for (j <- epNumsFile.getLines()) {
            val jlist = j.split(",").toList

            if (jlist(0) == cardname) {
              evalsBuff += jlist(1).toDouble
            }
          }
          epNumsFile.close()
        }
        evalsBuff.toList
      }
      val addition = (cardname, evals)
      distroBuff += addition
    }
    distroBuff.toList
  }

  //card, mu, sig
  def get_episode_params(distributions : List[(String, List[Double])]) : List[(String, Double, Double)] = {
    val paramsBuff = new ListBuffer[(String, Double, Double)]

    for (cardname <- cards) {
      for (i <- 0 until distributions.length) {
        if (distributions(i)._1 == cardname) {
          val vals : List[Double] = distributions(i)._2

          val mu : Double = vals.sum/vals.length

          val sig : Double = {
            val sumdiffsq: Double = {
              var inside: Double = 0.0
              for (j <- vals) {
                inside += math.pow(j - mu, 2)
              }
              inside
            }
            math.sqrt((sumdiffsq) / 29)
          }

          val addition = (cardname, mu, sig)
          paramsBuff += addition

        }
      }
    }
    paramsBuff.toList
  }

  def parse_hypotheses(file : String) : List[(String, Double, Double, Boolean)] = {
    //get hypothesis list from csv (CARD, mu, sig, flag)
    val filename = file
    val hypotheses = Source.fromFile(filename)

    //make into an array
    val hypoListBuff = new ListBuffer[(String, Double, Double, Boolean)]()
    for (line <- hypotheses.getLines) {
      val hypoSingle = line.split(",").map(_.trim)

      val hypo0 = hypoSingle(0)
      val hypo1 = hypoSingle(1).toDouble
      val hypo2 = hypoSingle(2).toDouble
      val hypo3 = hypoSingle(3).toBoolean

      val hypoFormatted = (hypo0, hypo1, hypo2, hypo3)

      hypoListBuff += hypoFormatted
    }

    hypoListBuff.toList

  }




  /**
   *
   * @param arg is a starting episode number
   *
   */
  def main(arg : Array[String]) : Unit = {

    val startLog = (arg(0) + "100").toInt

    //card distributions from single episode
    val distroParams : List[(String, Double, Double)] = get_episode_params(get_episode_points(startLog))


    for (i <- 0 until distroParams.length) {
      println(distroParams(i)._1 + "(mu= " + distroParams(i)._2 + ", SD= " + distroParams(i)._3 + ")")
    }
  }

}
