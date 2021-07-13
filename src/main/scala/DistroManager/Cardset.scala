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
    val ind = cards.indexOf(Card(card))
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

    val mu = mean(sample.toList)
    val sig = stdev(sample.toList)

    val buff = ListBuffer[(Card,Double)]()
    for(c<-cards) {
      //card, std devs away from mean
      buff.addOne( (c,(c.get_distro()._1-mu)/sig) )
    }
    buff.toList
  }

  def mean(values : List[Double]) : Double = {
    values.sum/values.length
  }

  def stdev(values : List[Double]) : Double = {
    val mu : Double = mean(values)
    val n : Int = values.length

    val sumdiffsq : Double = {
      var inside : Double = 0.0
      for (i <- values) {
        inside += math.pow((i - mu),2)
      }
      inside
    }

    math.sqrt(sumdiffsq/(n - 1))
  }

}
