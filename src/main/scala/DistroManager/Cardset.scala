package DistroManager

import Evaluation.Distribution
import org.apache.commons.math3.stat.inference.TTest

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import org.apache.commons.math3.stat.inference.TTest._

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
  def big_distribution() : (Double,Double) = {
    sample = List()
    for(c<-cards) {
      add_to_sample(c.sample)
    }

    (mean(sample), stdev(sample))
  }

  def big_distribution_minus_outliers() : (Double,Double) = {
    val outliers = get_outlier_scores()
    sample = List()
    for(o <- outliers) {
      if(o._2 < 2.0) {
        add_to_sample(o._1.sample)
      }
    }

    (mean(sample), stdev(sample))
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

  def find_synergies() : List[(CardInteraction, Double)]= {
    val data = DistributionManager.parse_alldata()

    val buff = ListBuffer[(CardInteraction, Double)]()

    for(a <- cards) {
      for(b <- cards) {
        if(a.cardname.compare(b.cardname) != 0) {
          val foo = CardInteraction(a, b, data.toSet)

          if(foo.is_an_interaction) {
            val outlier_score = get_outlier_score(List(foo.card_a, foo.card_b), foo.combined_power_mu)
            buff.addOne(foo, outlier_score)
          }

        }
      }
    }
    buff.toList
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
      buff.addOne( (c, get_outlier_score(List(c), c.mean(c.sample))) )
    }
    buff.toList
  }

  /**
   *
   * @return card, std dev from big mu
   */
  def get_outlier_score(kards : List[Card], mu : Double) : Double = {
    //fetch from alldata.csv all the mus
    sample = List()
    for(c<-cards) {
      var bool = false
      for(k <-kards) {
        if (c.cardname.compare(k.cardname) == 0) {
          bool = true
        }
      }
      if(!bool) {
        add_to_sample(c.sample)//maybe c.mean(c.sample)
      }
    }

    val delta_mu = mean(sample)
    val delta_sig = stdev(sample)

    //T TEST eventually lmaoo
    (mu-delta_mu)/delta_sig
  }

}
