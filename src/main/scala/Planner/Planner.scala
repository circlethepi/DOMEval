package Planner

import DistroManager.{Card, Hypothesis, HypothesisSet}

import scala.collection.mutable.ListBuffer

object Planner {

  //12 episodes per machine
  val machines = 3
  val episodespermachine = 12
  val MAX_EPISODES : Int = machines*episodespermachine
  val MIN_KINGDOM_SIZE = 2

  def Plan(hypotheses : HypothesisSet): List[List[Card]] = {

    //organize from least powerful to most powerful cards
    //episode with lower (...,4,5,6,...) eval cards
    //episode with uppper (...,4,5,6,...) eval cards

    val sorted = hypotheses.hypotheses.sortBy(_.mu)
    val ret = ListBuffer[List[Card]]()

    var currkingdom = MIN_KINGDOM_SIZE
    for(i <- 0 until MAX_EPISODES/2) {
      val lowkingdom = ListBuffer[Card]()
      val highkingdom = ListBuffer[Card]()

      for(s<-0 until currkingdom) {
        lowkingdom.addOne(sorted(s).card)
        highkingdom.addOne(sorted.reverse(s).card)
      }

      ret.addOne(lowkingdom.toList)
      ret.addOne(highkingdom.toList)
      if(sorted.size > currkingdom) {
        currkingdom += 1
      }
    }

    ret.toList
  }

}
