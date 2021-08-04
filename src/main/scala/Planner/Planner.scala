package Planner

import DistroManager.{Card, Hypothesis, HypothesisSet}
import Evaluation.Kingdom

import scala.collection.convert.ImplicitConversions.`collection asJava`
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

object Planner {

  //12 episodes per machine
  val machines = 3
  val episodespermachine = 10
  val MAX_EPISODES : Int = machines*episodespermachine
  val MIN_KINGDOM_SIZE = 2
  val MAX_KINGDOM_SIZE = 10

  def Plan(hypotheses : HypothesisSet): List[Kingdom] = {

    val mu_sorted = hypotheses.hypotheses.sortBy(_.mu).filterNot(_.card.cardname.indexOf("All") == 0)

    val sig_sorted = hypotheses.hypotheses.sortBy(_.sig).filterNot(_.card.cardname.indexOf("All") == 0)

    val foo = plan_from_list(mu_sorted)
    val bar = plan_from_list(sig_sorted)

    foo.concat(bar)
  }

  def plan_from_list(sorted : List[Hypothesis]) : List[Kingdom] = {

    val ret = ListBuffer[Kingdom]()

    for(i <- MIN_KINGDOM_SIZE to MAX_KINGDOM_SIZE) {
      val lowhalf =  {
        if(i > sorted.length/2) {
          sorted.slice(0, i)
        } else  {
          sorted.slice(0, sorted.length/2)
        }
      }
      val highhalf = {
        if(i > sorted.length/2) {
          sorted.slice(sorted.length - i, sorted.length + 1)
        } else  {
          sorted.slice(sorted.length/2, sorted.length)
        }
      }
      val lowkingdom = ListBuffer[Card]()
      val highkingdom = ListBuffer[Card]()
      val rand = new Random

      val lowbuff : mutable.Buffer[Hypothesis] = lowhalf.toBuffer
      val highbuff : mutable.Buffer[Hypothesis]= highhalf.toBuffer

      for(s<-0 until i) {
        val lowind = rand.nextInt(lowbuff.length)
        val highind = rand.nextInt(highbuff.length)

        lowkingdom.addOne(lowbuff(lowind).card)
        highkingdom.addOne(highbuff(highind).card)

        lowbuff.remove(lowind)
        highbuff.remove(highind)
      }

      ret.addOne(new Kingdom(lowkingdom.toList))
      ret.addOne(new Kingdom(highkingdom.toList))
    }

    ret.toList

  }


}
