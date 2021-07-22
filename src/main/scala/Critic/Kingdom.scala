package Critic

import DistroManager.Card

class Kingdom(cards : List[Card]) {

  override def toString: String = {
    var str = ""
    for(c<-cards) {
      str += c.cardname + ","
    }
    str.substring(0,str.length-1)
  }

}
