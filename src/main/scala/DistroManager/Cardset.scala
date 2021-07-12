package DistroManager

import scala.collection.mutable.ListBuffer
import scala.io.Source

case class Cardset(cardsetfile : String) {

  def cards : List[Card] = {
    val lines = Source.fromFile(cardsetfile).getLines()
    val buff = ListBuffer[Card]()
    for(l<-lines) {
      buff.addOne(Card(l.trim))
    }
    buff.toList
  }

  def add_data(card : String, kingdomsize : Int, power : Double): Unit = {
    val ind = cards.indexOf(card)
    cards(ind).tell_distrofile(kingdomsize,power)
  }

  /**
   *
   * @return card, std dev from big mu
   */
  def get_outliers() : List[(Card, Double)] = {
    val sample = ListBuffer[Double]()
    for(c<-cards) {
      sample.addOne(c.get_distro()._1)
    }


    List()
  }

}
