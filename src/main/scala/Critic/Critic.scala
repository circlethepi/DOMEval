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


  def critique(evaluation: EpisodeEvaluation, hypothesis: HypothesisSet): List[Critique] = {
    val critBuff = new ListBuffer[Critique]

    val critAdd = Critique(hypothesis, evaluation)

    critBuff += critAdd

    critBuff.toList
  }

  def main(arg: Array[String]): Unit = {
    println(arg(0))
  }


}

