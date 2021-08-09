import Critic.{Critic, Critique}
import DistroManager.DistributionManager.std_dev_range
import DistroManager.{Card, CardInteraction, DistributionManager, Hypothesis, HypothesisSet}
import Evaluation.{Kingdom, Reader}

import java.io.FileWriter
import scala.collection.mutable.ListBuffer
import scala.io.Source

object LoopController {

  /**
   *
   * @param args (1) is ASK ONLY bool (askoonly == 1 , parse_data != 1)
   *             args(1) is just ask boolean 0, data will be added, 1, no data will be added, but ask and plan will happen
   *             args(2) is previous hypothesis
   */
  def main(args : Array[String]) : Unit = {
    val int = args(0).toInt
    val str = args(1)
    if(int == 1) {
      //ask
      val ask_out = DistributionManager.ASK()
      val hypotesisset = ask_out._1
      val interactions = ask_out._2

      hypothesis_out(hypotesisset, str)
      interaction_out(interactions,str)

      //plan
      val plans = Planner.Planner.Plan(hypotesisset)

      planner_out(plans,str)
    } else {

      //evaluation
      val evaluations = Reader.Evaluate()

      //aquire hypothesis
      val prev_hypothesis = hypothesis_in(args(2))

      //critique
      val critiques: List[Critique] = {
        val buff = ListBuffer[Critique]()
        for (e <- evaluations) {
          buff.addOne(Critic.critique(e, prev_hypothesis))
        }
        buff.toList
      }

      //tell
      DistributionManager.TELL(critiques)

      //ask
      val ask_out = DistributionManager.ASK()
      val hypotesisset = ask_out._1
      val interactions = ask_out._2

      hypothesis_out(hypotesisset, str)
      interaction_out(interactions,str)

      //plan
      val plans = Planner.Planner.Plan(hypotesisset)

      planner_out(plans, str)
    }
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

  def hypothesis_out(hypothesis : HypothesisSet,str : String) : Unit = {
    val file = "hypothesis_" + str + ".csv"
    val fw = new FileWriter(file)

    fw write hypothesis.toString + "\n"

    fw.close()
  }

  def interaction_out(interactions : List[(CardInteraction, Double)],str : String) : Unit = {
    val file = "interactions_" + str + ".csv"
    val fw = new FileWriter(file)
    fw write "cards, mu_a, mu_b, z-score_a, z-score_b, mu avergae, interaction definition, z-score_ab, flagged\n"

    for(i<-interactions) {
      fw write i._1.toString + "," + i._2 + "," + (i._2 < -std_dev_range || i._2 > std_dev_range) + "\n"
    }

    fw.close()
  }

  def planner_out(kingdoms : List[Kingdom], str : String) : Unit = {
    val file = "planner_" + str + ".csv"
    val fw = new FileWriter(file)

    for(k <- kingdoms) {
      fw write k.toString + "\n"
    }

    fw.close()
  }
}
