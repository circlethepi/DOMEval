package Critic

import DistroManager._

import scala.collection.mutable.ListBuffer

object Critic {
  /**
   * Author: Merrick Ohata
   * Date: July 2021
   *
   * CRITIC for DOMINION
   *
  *
  * */

  def read_hypotheses() : Unit = {
    val h = Hypothesis(Card("test"))

  }

  def get_episode_distro(Array[String, Int]) : Unit = {
    val evals : List = {
      val evalsBuff : ListBuffer[Double] = ListBuffer[Nothing]
      for (i <- 0 until 30) {

    }
    }

  }




  /**
   *
   * @param arg is a starting log number
   *
   */
  def main(arg : Array[String]) : Unit = {
    val startlog : Int = arg(0).toInt

    get_episode_distro(startlog)

  }

}
