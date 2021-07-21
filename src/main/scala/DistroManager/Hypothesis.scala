package DistroManager

/**
 * Author: Casey Ford
 * July 2021
 *
 * @param card
 * @param outlier # of std devs away from sample mu it is, below a threshold it counts as an outlier
 */
case class Hypothesis(card : Card, outlier : Boolean ) {

  val mu  : Double = card.get_distro()._1
  val sig : Double = card.get_distro()._2

  override def toString : String = {
    "\nCard:" + card.cardname +
    "\nMu:" + mu +
    "\nSig:" + sig
  }

}
