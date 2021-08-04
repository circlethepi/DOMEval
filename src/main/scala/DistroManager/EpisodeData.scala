package DistroManager

import Evaluation.Kingdom

case class EpisodeData(kingdom : List[String], data : List[(String, (Double,Double))]) {

  def contains(card : String) : Boolean = {
    var bool = false
    for(k<-kingdom) {
      if(k.compare(card) == 0) {
        bool = true
      }
    }
    bool
  }

  def data_for(card : String) : Option[(Double,Double)] = {
    for(d<-data) {
      if(d._1.compare(card) == 0) {
        return Some(d._2)
      }
    }
    None
  }



}
