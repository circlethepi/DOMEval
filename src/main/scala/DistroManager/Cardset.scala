package DistroManager

import Evaluation.Distribution

import scala.collection.mutable.ListBuffer
import scala.io.Source

case class Cardset(cardsetfile : String) extends Distribution {

  //she keeps track of the BIG distribution of all card powers

  def cards : List[Card] = {
    val lines = Source.fromFile(cardsetfile).getLines()
    val buff = ListBuffer[Card]()
    for(l<-lines) {
      buff.addOne(Card(l.trim))
    }
    buff.toList
  }

  def add_data(card : String, kingdomsize : Int, power : Double): Unit = {
    val ind = cards.indexOf(Card(card))
    cards(ind).tell_distrofile(kingdomsize,power)
  }

  /**
   *
   * @return card, std dev from big mu
   */
  def get_outlier_scores() : List[(Card, Double)] = {
    val sample = ListBuffer[Double]()
    for(c<-cards) {
      sample.addOne(c.mean(c.sample))
    }

    val mu = mean(sample.toList)
    val sig = stdev(sample.toList)

    val buff = ListBuffer[(Card,Double)]()
    for(c<-cards) {
      //card, std devs away from mean
      buff.addOne( (c,(c.mean(c.sample)-mu)/sig) )
    }
    buff.toList
  }
}
