package Critic

import DistroManager._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import org.apache.commons._

object Critic {
  val cards: List[String] = {
    val cardses = Source.fromFile("cardset.txt").getLines.toList
    Source.fromFile("cardset.txt").close()

    cardses
  }

  /**
   * Author: Merrick Ohata
   * Date: July 2021
   *
   * CRITIC for DOMINION
  *
  * */

  /**
   * CONFIGURATION FUNCTIONS
   */

  def get_hypothesis_filename() : String = {
    "src/main/scala/hypothesisList.csv"
  }

  def get_significance_level() : Double = {
    //% are the probabilities that the mean differs by this amount purely by chance
    //1.65 //for 10%
    1.96 //for 5%
    //2.58 //for 1%
    //
  }

  /**
   * from a starting logNumber, collects all the evaluations of each card
   *
   * @param lognumStart starting logNumber
   * @return a list of (String, List(double)) values
   *         where the doubles are each of the evaluations from the episode
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

  /**
   * gets episode distribution information for each card
   *
   * @param distributions a list of (cardname, list of evaluations for an episode)
   * @return list of (card, mu, sigma) tuples for the episode
   **/

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

  /**
   * reads hypotheses from a file and makes them into a list
   *
   * @param file : String is the filename of the hypothesis file for the episode
   * @return list of (card, mu, sigma, outlier flag [boolean]) tuples
   **/
  def get_hypotheses(file : String) : List[(String, Double, Double, Boolean)] = {
    //get hypothesis list from csv (CARD, mu, sig, flag)
    val filename = file
    val hypotheses = Source.fromFile(filename)
    val hypoLines = hypotheses.getLines
    hypoLines.drop(1)

    //make into an array
    val hypoListBuff = new ListBuffer[(String, Double, Double, Boolean)]()
    for (line <- hypoLines) {
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
   * checks for true flag value and adds to list if true
   *
   * @param hypo : raw hypothesis list of tuples
   * @return list of outlier flagged cards
   **/

  def get_hypo_flags(hypo : List[(String, Double, Double, Boolean)]) : List[String] = {

    val flagBuff = new ListBuffer[String]()

    for (card <- hypo) {
      if (card._4 == true) {
        flagBuff += card._1
      }
    }

    flagBuff.toList
  }

  /**
   * gets rid of boolean bc we don't really care rn
   *
   * @param hypo : raw hypothesis list of tuples
   * @return list of (card, mu, sigma) tuples
   */
  def get_hypo_params(hypo : List[(String, Double, Double, Boolean)]) : List[(String, Double, Double)] = {
    val hypoParamsBuff = new ListBuffer[(String, Double, Double)]

    for (card <- hypo) {
      val buffAdd = (card._1, card._2, card._3)
      hypoParamsBuff += buffAdd
    }

    hypoParamsBuff.toList
  }

  /**
   * finds the number of standard deviations away from the hypothesis mean the episode mean is
   *
   * @param episodeData : episode data list of (card, mu, sigma) tuples
   * @param hypothesisData : hypothesis data list of (card, mu, sigma) tuples
   * @return list of (card, distance) tuples
   */
  def distance_caluclate(episodeData : List[(String, Double, Double)], hypothesisData: List[(String, Double, Double)]) : List[(String, Double)] = {

    val distBuff = new ListBuffer[(String, Double)]()

    for (distrEp <- episodeData) {
      for (distrHyp <- hypothesisData) {
        if (distrEp._1 == distrHyp._1) { //episode - hypothesis => negative means episode < hypothesis

          val cardname = distrEp._1
          val muEp = distrEp._2
          val muHyp = distrHyp._2
          val sigHyp = distrHyp._3

          //we calculate #SDs from hyp mean using hyp distr. info since hypothetically:
          //the episode samples should come from the hypothesis distribution
          //a wildly different result indicates either some sort of interaction or not enough about the distribution is known yet

          val numSDs = (muEp-muHyp) / sigHyp

          val add = (cardname, numSDs)

          distBuff += add

        }
      }
    }

    distBuff.toList

  }

  /**
   *
   * @param distances : List of (card, SD from hyp mean) tuples which we can get from distance_calculate
   * @return list of cards that should be flagged for being far away
   */
  def distance_flag(distances : List[(String, Double)]) : List[String] = {
    val flagBuff = new ListBuffer[String]()
    val flagSDCutOff = get_significance_level()

    for (card <- distances) {
      if (card._2 >= flagSDCutOff) {

        flagBuff += card._1

      }
    }
    flagBuff.toList
  }


  def annotate_hypothesis_set() : Unit = {
  "a"
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
    //"raw" hypotheses
    val hypotheses : List[(String, Double, Double, Boolean)] = get_hypotheses(get_hypothesis_filename())



    for (i <- 0 until distroParams.length) {
      println(distroParams(i)._1 + "(mu= " + distroParams(i)._2 + ", SD= " + distroParams(i)._3 + ")")
    }
  }

}
