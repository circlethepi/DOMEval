package Planner

object Planner {

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
