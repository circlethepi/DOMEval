package DistroManager

case class HypothesisSet(hypotheses : List[Hypothesis]) {

  override def toString: String = {
    var str = "card,mu,sig,outlier"
    for(h<-hypotheses) {
      str += h.toString
    }
    str
  }


}
