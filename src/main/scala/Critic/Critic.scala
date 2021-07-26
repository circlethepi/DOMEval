package Critic

import DistroManager._
import Evaluation.EpisodeEvaluation

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


  def critique(evaluation : EpisodeEvaluation, hypothesis : HypothesisSet) : Critique = {

      Critique(hypothesis, evaluation)

  }

  def main(arg : Array[String]) : Unit = {
    println("hello world")
  }

}
