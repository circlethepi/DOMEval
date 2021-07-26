package Evaluation

import DistroManager.Card

class Kingdom(val cards : List[Card]) {

  override def toString: String = {
    var str = ""
    for(c<-cards) {
      str += c.cardname + " "
    }
    str.substring(0,str.length-1)
  }

}
