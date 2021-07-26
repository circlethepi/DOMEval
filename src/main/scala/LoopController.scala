import Critic.{Critic, Critique}
import DistroManager.{Card, DistributionManager, Hypothesis, HypothesisSet}
import Evaluation.{Kingdom, Reader}

import java.io.FileWriter
import scala.collection.mutable.ListBuffer
import scala.io.Source

object LoopController {

  /**
   *
   * @param args (0) is current loop cnt
   */
  def main(args : Array[String]) : Unit = {

    val cnt = args(0).toInt

    //evaluation
    val evaluations = Reader.Evaluate()

    //aquire hypothesis
    val prev_hypothesis = hypothesis_in("hypothesisList.csv")

    //critique
    val critiques : List[Critique] =  {
      val buff = ListBuffer[Critique]()
      for(e<-evaluations) {
        buff.addOne(Critic.critique(e, prev_hypothesis))
      }
      buff.toList
    }

    //tell
    DistributionManager.TELL(critiques)

    //ask
    val hypothesisset = DistributionManager.ASK()

    hypothesis_out(hypothesisset, cnt)

    //plan
    val plans = Planner.Planner.Plan(hypothesisset)

    planner_out(plans,cnt)
  }

  def hypothesis_in(file : String) : HypothesisSet = {
    val lines = Source.fromFile(file).getLines()

    lines.drop(1)

    val hypos = ListBuffer[Hypothesis]()

    for(l<-lines) {
      val split = l.split(",")

      val card = Card(split(0))
      val mu = split(1).toDouble
      val sig = split(2).toDouble
      val outlier = split(3).toBoolean

      hypos.addOne(Hypothesis(card,mu,sig,outlier))
    }

    HypothesisSet(hypos.toList)
  }

  def hypothesis_out(hypothesis : HypothesisSet, cnt : Int) : Unit = {
    val file = "hypothesis_" + cnt + ".csv"
    val fw = new FileWriter(file)

    fw write hypothesis.toString

    fw.close()
  }

  def planner_out(kingdoms : List[Kingdom], cnt : Int) : Unit = {
    val file = "planner_" + cnt + ".csv"
    val fw = new FileWriter(file)

    for(k <- kingdoms) {
      fw write k.toString + "\n"
    }

    fw.close()
  }
}
