package Evaluation

import DistroManager.Card

import java.io.{File, PrintWriter}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.matching.Regex
import Evaluation.EpisodeEvaluation

/**
 * Author: Merrick Ohata
 * Date: July 2021
 *
 * READER for DOMINION
 * Parses game data to generate evaluations of card influence
 * takes lognumber and number of games played to generate evaluations for each game
 * hi idk how to use scala so bear with my stupid valbuffer usages
 */

object Reader {

  /*   SETUP    */
  //Cards used in the game
  val cards: List[String] = {
    Source.fromFile("cardset.txt").getLines.toList
  }
  val gameCards : ListBuffer[String] = ListBuffer()

  //info from move history - number of plays per card for each deck
  val plays0: ArrayBuffer[(String, Int)] = ArrayBuffer()
  val plays1: ArrayBuffer[(String, Int)] = ArrayBuffer()

  //info from game log - number of copies of each card in each deck
  val deck0: ArrayBuffer[(String, Int)] = ArrayBuffer()
  val deck1: ArrayBuffer[(String, Int)] = ArrayBuffer()

  val gameScore: ArrayBuffer[Int] = ArrayBuffer(0, 0)
  var gameTurns: Int = 0

  //card evaluation holder
  val cardEvals: ArrayBuffer[(Card, Double)] = ArrayBuffer()

  //************************************************
  //**LIST PLAY FREQUENCY**
  //Maps each element of a list to integer count of occurences
  //************************************************
  def list_Play_Freq[A](list1: List[A]): Map[A, Int] = {
    list1.groupBy(el => el).map(e => (e._1, e._2.length))
  }


  //********************************************************
  //**PARSE MOVE HISTORY**
  //Parses move history for each deck
  //outputs values for play frequency for each card for each player
  //***************************************************************
  /**
   * PARSE MOVE HISTORY
   * Parses move history for each deck
   *
   * @param lognum number of file to read from
   * @return
   */
  def parse_moveHistory(lognum: Int): Unit = {
    val filename = "Data/moveHistory" + lognum + ".txt"

    //getting plays from move history file
    val mvHist = io.Source.fromFile(filename)
    val mvHistStr = mvHist.getLines.mkString //turn file into string
    val mvHistList = Source.fromFile(filename).getLines.toList
    mvHist.close()

    //getting list of cards in game
    val cardsList = mvHistList(1).split(" ").map(_.trim).toList
    for (i <- 0 until cardsList.length) {
      gameCards += cardsList(i).toLowerCase.capitalize
    }

    //tokenizers - getting plays for each player
    //player0
    val p0ListBuff: ListBuffer[String] = ListBuffer()
    val p0Pattern: Regex = "Player0-Played ([a-zA-Z-]+)".r

    for (play <- p0Pattern.findAllMatchIn(mvHistStr)) { //adding plays to ListBuffer
      p0ListBuff += s"${play.group(1)}"
    }

    val p0List = p0ListBuff.toList //turn buffer into list
    val p0FreqMap = list_Play_Freq(p0List) //getting frequencies
    val p0Freq = p0FreqMap.toArray //getting pairs

    for (i <- 0 until p0Freq.length) {
      plays0 += p0Freq(i) //Array buffer of  frequencies for PLAYER 0
    }


    //player1
    val p1ListBuff: ListBuffer[String] = ListBuffer()
    val p1Pattern: Regex = "Player1-Played ([a-zA-Z-]+)".r

    for (play <- p1Pattern.findAllMatchIn(mvHistStr)) { //adding plays to ListBuffer
      p1ListBuff += s"${play.group(1)}"
    }
    val p1List = p1ListBuff.toList //turn buffer into list
    val p1FreqMap = list_Play_Freq(p1List) //getting frequencies
    val p1Freq = p1FreqMap.toArray //getting pairs

    for (i <- 0 until p1Freq.length) {
      plays1 += p1Freq(i) //array buffer of frequencies FOR PLAYER 1
    }

  }


  //*********************************************************
  //**PARSE GAME LOG**
  //Parses game log
  //outputs values for gameScore, gameTurns, and alphabetically sorted decklists of actions, deck0 deck1
  //***************************************************************************
  def parse_log(lognum: Int): Unit = {
    val filename = "Data/log" + lognum + ".csv"
    val gamelog = Source.fromFile(filename)


    //make into an array
    val logArray = new ArrayBuffer[Array[String]]()
    for (line <- gamelog.getLines) {
      logArray += line.split(",").map(_.trim)
    }
    gamelog.close()

    //get field names and values for decks
    val heads = logArray(0) //getting first row
    val tails = logArray(logArray.length - 1) //getting last row
    val tailsInt = tails.map(_.toInt) //making into ints vs strng
    val finalState = (heads zip tailsInt).toMap.toArray.sorted //sorted pairs sim mvhist


    //getting decklists and final scores
    for (i <- 0 until tailsInt.length) {
      val slotName = finalState(i)._1: String
      val slotVal = finalState(i)._2: Int

      if (slotName == "p0Score") {
        gameScore(0) = slotVal: Int //getting p0 final score
        //  println("p0Score = " + gameScore(0))

      } else {
        if (slotName == "p1Score") {
          gameScore(1) = slotVal: Int //getting p1 final score
          //   println("p1Score = " + gameScore(1))

        } else {
          if (slotName == "turn") {
            gameTurns = slotVal: Int
            //  println("numTurns = " + gameTurns)

          } else { //generating deck lists otherwise

            val p0DeckPat: Regex = "p0([a-zA-Z]+)".r
            val p1DeckPat: Regex = "p1([a-zA-Z]+)".r
            val nonActionCards: List[String] = List("Copper", "Silver", "Gold", "Curse", "Estate", "Duchy", "Province")
            val addMaybe = (slotName.substring(2): String, slotVal: Int)

            if ((p0DeckPat.pattern.matcher(slotName).matches == true) && (nonActionCards.contains(slotName.substring(2)) == false)) {
              deck0 += addMaybe //if card is action and from deck 0, add to deck 0
              // println( addMaybe._1 + ", " + addMaybe._2)
            } else {
              if ((p1DeckPat.pattern.matcher(slotName).matches == true) && (nonActionCards.contains(slotName.substring(2)) == false)) {
                deck1 += addMaybe //if card is action and from deck 1, add to deck 1
                //  println( addMaybe._1 + ", " + addMaybe._2)
              }
            }
          }
        }
      }
    }
  }


  //**********************************************************************
  //**CALCULATE SCORES**
  //uses calculated values and parsing to calculate the evaluation scores of each card
  //for each card: for each deck: %VP(plays/copies/turn)
  //************************************************************************
  def calculate_values(lognum: Int): Unit = {
    val vpMultiplier0 = gameScore(0).toDouble / (gameScore(0) + gameScore(1))
    val vpMultiplier1 = gameScore(1).toDouble / (gameScore(0) + gameScore(1))

    //making array of values
    for (i <- cards.indices) {
      val cardCurrent = cards(i) : String

      //                              plays0num, deck0num, plays1num, deck1num
      val cardVals: ArrayBuffer[Int] = ArrayBuffer(0, 0, 0, 0) //setting up card calculation
      var cardCalc: Double = 0.0

      for (i <- plays0.indices) { //getting plays0num
        if (plays0(i)._1 == cardCurrent) {
          cardVals(0) = plays0(i)._2
          //println(cardCurrent + " plays0: " + cardVals(0))
        }
      }

      for (i <- deck0.indices) { //getting deck0num
        if (deck0(i)._1 == cardCurrent) {
          cardVals(1) = deck0(i)._2
          //println(cardCurrent + " deck0: " + cardVals(1))
        }
      }

      for (i <- plays1.indices) { //getting plays1num
        if (plays1(i)._1 == cardCurrent) {
          cardVals(2) = plays1(i)._2
          // println(cardCurrent + " plays1: " + cardVals(2))
        }
      }

      for (i <- deck1.indices) { //getting deck1num
        if (deck1(i)._1 == cardCurrent) {
          cardVals(3) = deck1(i)._2
          //println(cardCurrent + " deck1: " + cardVals(3))
        }
      }

      //calculation
      var deck0Eval: Double = 0.0
      var deck1Eval: Double = 0.0

      //deck0
      val deck0EvalNum = vpMultiplier0 * cardVals(0)
      val deck0EvalDenom = gameTurns * cardVals(1)

      if (deck0EvalDenom != 0.0) {
        deck0Eval = deck0EvalNum / deck0EvalDenom //if not undefined, calculate deck eval
      }

      //deck1
      val deck1EvalNum = vpMultiplier1 * cardVals(2)
      val deck1EvalDenom = gameTurns * cardVals(3)

      if (deck1EvalDenom != 0.0) {
        deck1Eval = deck1EvalNum / deck1EvalDenom //if not undefined, calculate deck eval
      }

      var vpAdd : Double = 0.0;
      //winning deck addition
      if (gameScore(0) > gameScore(1)) {
        vpAdd = vpMultiplier0 * deck0(i)._2
      } else {
        vpAdd = vpMultiplier1 * deck1(i)._2
      }


      //* check here to see whether or not the card was in the pool*
      if (gameCards.contains(cardCurrent)) {
        cardCalc = 100 * (deck0Eval + deck1Eval) + vpAdd : Double
        val cardCard = new Card(cardCurrent)
        val cardCurrentEval = (cardCard: Card, cardCalc : Double) //get card, score pair
        cardEvals += cardCurrentEval //add to list
      }

    }
    //sanity check terminal
      println("\nLOG: " + lognum)
        for (i <- cardEvals.indices) {
          println(cardEvals(i)._1 + ", " + cardEvals(i)._2)
        }
  }

  def get_inputfiles() : String = {
    "episodeNumberList.txt"
  }

  /**
   *
   * takes no input, just reads the file
   *
   */
  def Evaluate(): List[EpisodeEvaluation] = {

    val filename = get_inputfiles()

      //getting list of starting numbers from text file of episode starts

    val epEvalsBuff = ListBuffer[EpisodeEvaluation]()

    val epNumsFile = io.Source.fromFile(filename)
    for (startLog <- epNumsFile.getLines()) {

      val startLogString = startLog + "100"
      val startingNum: Int = startLogString.toInt
      val numGames: Int = 30 //inputs(1).toInt

      val gameEvalsBuff = new ListBuffer[GameEvaluation]()

      for (i <- 0 until numGames) { //starting at a given number, go for a number of games
        val logNumber = startingNum + i
        //clear global variables
        plays0.clear()
        plays1.clear()
        deck0.clear()
        deck1.clear()
        gameScore(0) = 0
        gameScore(1) = 0
        gameTurns = 0
        cardEvals.clear()
        parse_moveHistory(logNumber)
        parse_log(logNumber)
        calculate_values(logNumber)
        //WRITE TO CSV FILE
        //creating the file
        val fileName = "cardEvaluations" + logNumber
        val fileObject = new File("evaluations/" + fileName + ".csv")
        fileObject.createNewFile() // Creating a file
        val writer = new PrintWriter(fileObject) // Passing reference of file to the printwriter

        //writing the data
        writer.write("CARD, EVALUE")

        //making GameEvaluation
        val gameEval = GameEvaluation(cardEvals.toList)
        gameEvalsBuff.addOne(gameEval)

        for (i <- cardEvals.indices) {
          writer.write("\n" + cardEvals(i)._1 + ", " + cardEvals(i)._2)
        }
        writer.close() // Closing printwriter
      }

      epEvalsBuff.addOne(EpisodeEvaluation(gameEvalsBuff.head.get_kingdom(), gameEvalsBuff.toList))
    }

    epNumsFile.close()
    epEvalsBuff.toList
  }
}
