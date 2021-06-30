import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer


object Reader extends App {

  val cards : List[String] = List("Bureaucrat",
    "Festival",
    "Gardens",
    "Laboratory",
    "Market",
    "Moneylender",
    "Remodel",
    "Smithy",
    "Thief",
    "Village",
    "Witch",
    "Woodcutter")

  val deck0 : ArrayBuffer[(String, Int)] = ArrayBuffer()
  val deck1 : ArrayBuffer[(String, Int)] = ArrayBuffer()
  val gameScore : ArrayBuffer[Int] = ArrayBuffer(0,0)


  //Maps each element of a list to integer count of occurences
  def  list_Play_Freq[A](list1:List[A]):Map[A, Int] = {
    list1.groupBy(el => el).map(e => (e._1, e._2.length))
  }

  //**PARSE MOVE HISTORY**
  //Parses move history for each deck
  //outputs (p0FreqArray, p1FreqArray), both Array[(card, numplays)] sorted alphabetically
  def parse_moveHistory(lognum : Int) : Unit = {
    val filename = "moveHistory" + lognum + ".txt"

    //getting plays from move history file
    val mvHist = Source.fromFile(filename)
    val mvHistStr = mvHist.getLines.mkString //turn file into string
    //val mvLines: List[String] = mvHist.getLines.toList
    //val numlines = mvLines.length
    mvHist.close()


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
    val p0FreqArray = p0FreqSort.toArray  //Array of sorted frequencies for PLAYER 0

    //println(p0FreqArray)

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
    val p1FreqArray = p1FreqSort.toArray  //list of sorted frequencies FOR PLAYER 1

    //print(p1FreqArray)

    return (p0FreqArray, p1FreqArray)

  }

  //**PARSE GAME LOG**
  //Parses game log
  //edits global variables gameScore, final scores of each player and alphabetically sorted decklists of actions, deck0 deck1
  def parse_log(lognum : Int) : Unit = {
    val filename = "log" + lognum + ".csv"
    val gamelog = Source.fromFile(filename)


    //make into an array
    val logArray = new ArrayBuffer[Array[String]]()
    for (line <- gamelog.getLines) {
      logArray += line.split(",").map(_.trim)
    }
    gamelog.close()

    //get field names and values for decks
    val heads = logArray(0)                            //getting first row
    val tails = logArray(logArray.length - 1)           //getting last row
    val tailsInt = tails.map(_.toInt)                           //making into ints vs strng
    val finalState = (heads zip tailsInt).toMap.toArray.sorted  //sorted pairs sim mvhist


    //getting decklists and final scores
    for ( i <- 0 until tailsInt.length) {
      val slotName = finalState(i)._1 : String
      val slotVal = finalState(i)._2 : Int

      if (slotName == "p0Score") {
        gameScore(0) = slotVal: Int        //getting p0 final score
      } else {
        if (slotName == "p1Score") {
          gameScore(1) = slotVal : Int     //getting p1 final score
        } else {     //generating deck lists otherwise

          val p0DeckPat : Regex = "p0([a-zA-Z]+)".r
          val p1DeckPat : Regex = "p1([a-zA-Z]+)".r
          val nonActionCards : List[String] = List("Copper", "Silver", "Gold", "Curse", "Estate", "Duchy", "Province")
          val addMaybe = (slotName.substring(2): String, slotVal : Int)

          if ((p0DeckPat.pattern.matcher(slotName).matches == true) && (nonActionCards.contains(slotName.substring(2)) == false)) {
            deck0 += addMaybe                     //if card is action and from deck 0, add to deck 0
            //println( addMaybe._1 + ", " + addMaybe._2)
          } else {
            if ((p1DeckPat.pattern.matcher(slotName).matches == true) && (nonActionCards.contains(slotName.substring(2)) == false)) {
              deck1 += addMaybe                   //if card is action and from deck 1, add to deck 1
              //println( addMaybe._1 + ", " + addMaybe._2)
            }
          }
        }
      }
    }

    //GAME SUMMARY/sanity check
//    println("FINAL SCORE:\nPlayer 0: " + gameScore(0) + "\nPlayer 1: " + gameScore(1) + "\n\nPlayer 0 DECKLIST:")
//    for (i <- 0 until (deck0.length) ) {
//      println(deck0(i))
//    }
//    println("\n\nPlayer 1 DECKLIST:")
//    for (i <- 0 until (deck1.length) ) {
//      println(deck1(i))
//    }

  }

  //**CALCULATE SCORES**




    def main() : Unit = {
      val mvHist = Source.fromFile("moveHistory100.txt")
      val line = mvHist.getLines.take(1).toList
      mvHist.close()
      println(line)

      //write to a file
    }

  parse_moveHistory(100)
  parse_log(100)

}
