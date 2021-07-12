package DistroManager

/**
 * Author: Casey Ford
 * July 2021
 *
 * @param card
 * @param outlier
 */
case class Hypothesis(card : Card, outlier : Option[Double]) {

  val mu  : Double = card.get_distro()._1
  val sig : Double = card.get_distro()._2

  override def toString : String = {
    "\nCard:" + card.cardname +
    "\nMu:" + mu +
    "\nSig:" + sig
  }

}
