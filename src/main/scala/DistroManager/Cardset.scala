package DistroManager

import Evaluation.Distribution

import scala.collection.mutable.ListBuffer
import scala.io.Source

case class Cardset(cardsetfile : String) extends Distribution {

  //she keeps track of the BIG distribution of all card powers

  val cards : List[Card] = {
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

  def parse_cards() : Unit = {
    for(c<-cards) {
      c.parse_distrofile()
    }
  }

  /**
   *
   * @return card, std dev from big mu
   */
  def get_outlier_scores() : List[(Card, Double)] = {
    //fetch from alldata.csv all the mus
    val buff = ListBuffer[(Card,Double)]()
    for(c<-cards) {
      //card, std devs away from mean
      buff.addOne( (c, get_outlier_score(List(c))) )
    }
    buff.toList
  }

  /**
   *
   * @return card, std dev from big mu
   */
  def get_outlier_score(kards : List[Card]) : Double = {
    //fetch from alldata.csv all the mus
    for(c<-cards) {
      var bool = false
      for(k <-kards) {
        if (c.cardname.compare(k.cardname) == 0) {
          bool = true
        }
      }
      if(!bool) {
        add_to_sample(c.sample)
      }
    }

    val delta_mu = mean(sample)
    val delta_sig = stdev(sample)

    val kappa_sample = ListBuffer[Double]()
    for(k<-kards) {
      kappa_sample.addAll(k.sample)
    }

    val kappa_mu = mean(kappa_sample.toList)
    val kappa_sig = stdev(kappa_sample.toList)

    //T TEST eventually lmaoo
    (kappa_mu-delta_mu)/delta_sig
  }

}
