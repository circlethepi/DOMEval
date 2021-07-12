package DistroManager

case class Hypothesis(card : Card) {

  override def toString : String = {
    val distro = card.get_distro()

    "\nCard:" + card.cardname +
    "\nMu:" + distro._1 +
    "\nSig:" + distro._2
  }

}
