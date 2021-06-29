import scala.io.Source
import scala.util.matching.Regex

object Reader extends App {

  def parse_moveHistory(lognum : Int) : Unit = {
    val filename = "moveHistory" + lognum + ".txt"

    //what do you want from this? -> for each player, non-money plays
    val mvHist = Source.fromFile(filename)
    val mvHistStr = mvHist.getLines.mkString
    val mvLines: List[String] = mvHist.getLines.toList
    val numlines = mvLines.length

    //tokenizers
    val p0Pattern: Regex = "Player0-Played ([a-zA-Z-]+)".r

    for (p0Plays <- p0Pattern.findAllMatchIn(mvHistStr))
      println(s"${p0Plays.group(1)}")

    val p1Pattern: Regex = "Player1-Played ([a-zA-Z-]+)".r

    for (p1Plays <- p1Pattern.findAllMatchIn(mvHistStr))
      println(s"${p1Plays.group(1)}")


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
