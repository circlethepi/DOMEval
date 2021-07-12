package DistroManager

import java.io.{File, FileWriter}
import scala.collection.mutable.ListBuffer
import scala.io.Source

object DistributionManager {

  val distrocols : Array[String] = Array("kingdomsize","cardvalue")

  def initialize_distributions(configfile : String) : Boolean = {
    val lines = Source.fromFile(configfile).getLines()
    for(l <- lines) {
      val filename = get_distro_filename(l)
      val file = new File(filename)
      //if file exists we need not do anything, and SHOULD not do anything
      if(file.createNewFile()) {
        val filewrite = new FileWriter(file)
        filewrite write "kingdomsize,cardvalue\n" // <INT,DOUBLE,INT,INT>
        filewrite.close()
      }
    }

    true
  }

  def get_distro_filename(card : String) : String = {
    card + "_distro.csv"
  }

  /**
   *
   * @param datafile
   */
  def TELL(datafile : String) : Unit = {
    val lines = Source.fromFile(datafile).getLines()
    lines.drop(1) //header

    val buff = ListBuffer[(Card,Double)]()
    for(l<-lines) {
      val splt = l.split(",")
      val card = Card(splt(0))
      val power = splt(1).toDouble

      buff.addOne((card,power))
    }

    for(b<-buff) {
      b._1.tell_distrofile(buff.size,b._2)
    }

  }

  def ASK() : List[Hypothesis] = {


    List()
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
