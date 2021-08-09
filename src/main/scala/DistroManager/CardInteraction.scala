package DistroManager

import org.apache.commons.math3.stat.inference.TTest

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

case class CardInteraction(val card_a : Card, val card_b : Card, data : Set[EpisodeData]) {

  val ztest_threshold = 2.58

  val a_data : Set[EpisodeData] = episodes_containing(card_a.cardname,data)
  val b_data : Set[EpisodeData] = episodes_containing(card_b.cardname,data)

  val a_not_b_alldata : Set[EpisodeData] = a_data -- b_data
  val b_not_a_alldata : Set[EpisodeData] = b_data -- a_data
  val a_and_b_alldata : Set[EpisodeData] = a_data & b_data

  val a_not_b : List[Double] = data_from(card_a.cardname,a_not_b_alldata)
  val b_not_a : List[Double] = data_from(card_b.cardname,b_not_a_alldata)
  val a_with_b : List[Double] = data_from(card_a.cardname,a_and_b_alldata)
  val b_with_a : List[Double] = data_from(card_b.cardname,a_and_b_alldata)

  val a_z_score : Double = (mean(a_with_b)  - mean(a_not_b)) /  stdev(a_not_b)
  val b_z_score : Double = (mean(b_with_a)  - mean(b_not_a)) /  stdev(b_not_a)

  val is_an_interaction : Boolean = {
    a_z_score < -ztest_threshold || b_z_score < -ztest_threshold ||
      a_z_score > ztest_threshold || b_z_score > ztest_threshold
  }

  val interaction_definition : String = {

    if (is_an_interaction) {

      val a_str : String = {
        if (a_z_score < -ztest_threshold) {
          "negative"
        } else if (a_z_score > ztest_threshold) {
          "positive"
        } else {
          "neutral" //aka no interaction
        }
      }

      val b_str : String = {
        if (b_z_score < -ztest_threshold) {
          "negative"
        } else if (b_z_score > ztest_threshold) {
          "positive"
        } else {
          "neutral" //aka no interaction
        }
      }

      a_str + "-" + b_str

    } else {
      "no interaction" //aka no interaction
    }

  }

  val combined_power_mu : Double = (mean(a_with_b) + mean(b_with_a)) / 2

  def episodes_containing(card : String, data : Set[EpisodeData]) : Set[EpisodeData] = {
    var buffer = Set[EpisodeData]()
    for(d<-data) {
      if(d.contains(card)) {
        buffer = buffer + d
      }
    }

    buffer
  }

  def data_from(card : String, data : Set[EpisodeData]) : List[Double] = {
    val buffer = ListBuffer[Double]()
    for(d<-data) {
      buffer.addOne(d.data_for(card).get._1)
    }
    buffer.toList
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

  override def toString: String = {
    card_a.toString + " " + card_b.toString +
      "," + mean(a_with_b) + "," + mean(b_with_a) +
      "," + a_z_score + "," + b_z_score +
      "," + combined_power_mu +
      "," + interaction_definition
  }

}

