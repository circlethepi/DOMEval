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
      currkingdom += 1
    }

    ret.toList
  }

  //reeeeeEEEEEEEEEEEE

  //maybe constructs a set of episodes to test a specific card?
  //like ~10-30 depending on computer space, maybe in priority order?

  //so we see that card A has evaluated as poorly explored, so we generate
  //10-15 episodes without it
  //10-15 episodes with it
  //and expect it to be better understood

  //alt we see that card B has evaluated well above the mean, so we generate
  //a kingdom where B is the only card, (expect to see B dominate / preform as it has been)
  //a number of small kingdoms where B is rated as the most powerful, (expect to see B dominate / preform as it has been)
  //a kingdom with every card rated below B (expect to see B dominate / preform as it has been)

  //alt we see that card C has evaluated well below the mean, so we generate
  //a kingdom where C is the only card, (expect the winner to be essentially random, implying C's impact on the game is minimal)
  //a number of small kingdoms where C is the lowest rated card, but no card is ranked exceptionally high (expect other cards to dominate C in buys/plays)

}
