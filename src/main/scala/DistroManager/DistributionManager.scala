package DistroManager

import java.io.{File, FileWriter}
import scala.io.Source

object DistributionManager {

  val distrocols : Array[String] = Array("kingdomsize","cardvalue","played","bought")

  def initialize_distributions(configfile : String) : Boolean = {
    val lines = Source.fromFile(configfile).getLines()
    for(l <- lines) {
      val filename = get_distro_filename(l)
      val file = new File(filename)
      //if file exists we need not do anything, and SHOULD not do anything
      if(file.createNewFile()) {
        val filewrite = new FileWriter(file)
        filewrite write "kingdomsize,cardvalue,played,bought\n" // <INT>
        filewrite.close()
      }
    }

    true
  }

  def get_distro_filename(card : String) : String = {
    card + "_distro.csv"
  }

  def TELL(datafile : String) : Unit = {

  }

  def ASK() : Unit = {

  }

  /**
   * @param args args(0) INIT TELL or ASK
   *             INIT args(10 is configfile (list of elements for distributions)
   *             TELL args(1) is datafile
   *             ASK takes no arguments
   */
  def main(args:Array[String]) : Unit = {
      args(0) match {
        case "INIT" => initialize_distributions(args(1))
        case "TELL" => TELL(args(1))
        case "ASK" => ASK()
      }
  }

}