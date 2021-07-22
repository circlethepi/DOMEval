package DistroManager

/**
 * Author: Casey Ford
 * July 2021
 *
 * @param card
 * @param outlier # of std devs away from sample mu it is, below a threshold it counts as an outlier
 */
case class Hypothesis(card : Card, mu : Double, sig : Double, outlier : Boolean ) {

  override def toString : String = {
    "\n" + card.cardname + "," + mu + "," + sig + "," + outlier
  }

}