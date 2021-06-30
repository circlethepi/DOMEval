import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer


object Reader extends App {

  //Maps each element of a list to integer count of occurences
  def  list_Play_Freq[A](list1:List[A]):Map[A, Int] = {
    list1.groupBy(el => el).map(e => (e._1, e._2.length))
  }

  //Parses move history into (card, #times played) pairs for each deck
  def parse_moveHistory(lognum : Int) : Unit = {
    val filename = "moveHistory" + lognum + ".txt"

    //getting plays from move history file
    val mvHist = Source.fromFile(filename)
    val mvHistStr = mvHist.getLines.mkString //turn file into string
    //val mvLines: List[String] = mvHist.getLines.toList
    //val numlines = mvLines.length


    //tokenizers - getting plays for each player
      //player0
    val p0ListBuff = new ListBuffer[String]()
    val p0Pattern: Regex = "Player0-Played ([a-zA-Z-]+)".r

    for (play <- p0Pattern.findAllMatchIn(mvHistStr)) {  //adding plays to ListBuffer
      p0ListBuff += s"${play.group(1)}"
    }

    val p0List = p0ListBuff.toList            //turn buffer into list
    val p0FreqMap = list_Play_Freq(p0List)       //getting frequencies
    val p0Freq = p0FreqMap.toArray              //getting pairs

    var p0FreqSort = new ListBuffer[(String, Int)]() //sorting alphabetically
    for (card <- p0Freq) {
      p0FreqSort += card
    }

    p0FreqSort = p0FreqSort.sorted
    val p0FreqList = p0FreqSort.toList  //list of sorted frequencies for PLAYER 0

    //print(p0FreqList)


      //player1
    val p1ListBuff = new ListBuffer[String]()
    val p1Pattern: Regex = "Player1-Played ([a-zA-Z-]+)".r

    for (play <- p1Pattern.findAllMatchIn(mvHistStr)) {  //adding plays to ListBuffer
     p1ListBuff += s"${play.group(1)}"
    }
    val p1List = p1ListBuff.toList            //turn buffer into list
    val p1FreqMap = list_Play_Freq(p1List)       //getting frequencies
    val p1Freq = p1FreqMap.toArray              //getting pairs

    var p1FreqSort = new ListBuffer[(String, Int)]() //sorting alphabetically
    for (card <- p1Freq) {
      p1FreqSort += card
    }

    p1FreqSort = p1FreqSort.sorted
    val p1FreqList = p1FreqSort.toList  //list of sorted frequencies FOR PLAYER 1

    //print(p1FreqList)

  }

    def main() : Unit = {
      val mvHist = Source.fromFile("moveHistory100.txt")
      val line = mvHist.getLines.take(1).toList
      mvHist.close()
      println(line)

      //write to a file
    }

  parse_moveHistory(100)
}
