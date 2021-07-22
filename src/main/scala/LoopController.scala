import Critic.Critic
import DistroManager.DistributionManager
import Evaluation.Reader

object LoopController {

  def main(args : Array[String]) : Unit = {

    //evaluation
    val evaluations = Reader.Evaluate()

    //critique
    val critiques = Critic.critique(evaluations)

    //tell
    DistributionManager.TELL(critiques)

    //ask
    val hypothesisset = DistributionManager.ASK()

    //plan
    Planner.Planner.Plan(hypothesisset)
  }

}
