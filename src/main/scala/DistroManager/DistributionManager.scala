package DistroManager

import Critic.Critique

import java.io.{File, FileWriter}
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Author: Casey Ford
 * July 2021
 */
object DistributionManager {

  val distrocols : Array[String] = Array("kingdomsize","cardvalue")

  val std_dev_range = 2.58

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

    val filename = get_bigdata_filename()
    val file = new File(filename)
    //if file exists we need not do anything, and SHOULD not do anything
    if(file.createNewFile()) {
      val filewrite = new FileWriter(file)
      filewrite write "kingdom,cardvalues,critiques\n" // <INT,DOUBLE,INT,INT>
      filewrite.close()
    }

    true
  }

  def get_bigdata_filename() : String = {
    "alldata.csv"
  }

  def get_distro_filename(card : String) : String = {
    card + "_distro.csv"
  }

  def get_baseset_filename() : String = {
    "cardset.txt"
  }

  /**
   *
   * @param datafile
   */
  def add_data_from_file(datafile : String) : Unit = {
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

  def add_data(critiques : List[Critique]) : Unit = {

  }

  /**
   * adds data from all files listed in config file to relevant distributions
   *
   * @param configfile file with list of datafiles
   */
  def TELL(configfile : String) : Unit = {
    val files = Source.fromFile(configfile).getLines()
    for(f<-files) {
      add_data_from_file(f)
    }
  }

  def ASK() : HypothesisSet = {
    val cardset = Cardset(get_baseset_filename()).get_outlier_scores()
    val H = ListBuffer[Hypothesis]()
    for(c<-cardset) {
      val distro = c._1.get_distro()
      H.addOne(Hypothesis(c._1, distro._1,distro._2, c._2 < std_dev_range*(-1) || c._2 > std_dev_range))
    }

    HypothesisSet(H.toList)
  }



  /**
   * @param args args(0) INIT TELL or ASK
   *             INIT args(1) is configfile (list of elements for distributions)
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
