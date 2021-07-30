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

  val outputpath = "src/main/scala/DistroManager/Distributions/"

  val distrocols : Array[String] = Array("kingdomsize","cardvalue")

  val std_dev_range = 2.00

  def initialize_distributions(configfile : String) : Boolean = {
    val lines = Source.fromFile(configfile).getLines()
    for(l <- lines) {
      val filename = get_distro_filename(l)
      val file = new File(filename)
      //if file exists we need not do anything, and SHOULD not do anything
      if(file.createNewFile()) {
        val filewrite = new FileWriter(file)
        filewrite write "cardvalue\n" // <INT,DOUBLE,INT,INT>
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
    outputpath + "alldata.csv"
  }

  def get_distro_filename(card : String) : String = {
    outputpath + card + "_distro.csv"
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
    val filename = get_bigdata_filename()
    val fw = new FileWriter(new File(filename),true)
    for(c<-critiques) {
      val kingdom = c.evaluation.kingdom
      val eval = c.evaluation
      fw write kingdom.toString + "," + eval.toString + "\n"

      //each individual card distribution from each individual game eval
      for(c <- eval.cardDistributions) {
        val filename_c = get_distro_filename(c.cardname)
        val fw_c = new FileWriter(new File(filename_c), true)
        fw_c write + c.mean(c.sample) + "," + c.stdev(c.sample) + "\n"
        fw_c.close()
      }
    }
    fw.close()
  }

  /**
   * adds data from all files listed in config file to relevant distributions
   *
   * @param configfile file with list of datafiles
   */
  def TELL(critiques : List[Critique]) : Unit = {
    add_data(critiques)
  }

  def ASK() : HypothesisSet = {
    val cardset = Cardset(get_baseset_filename())
    cardset.parse_cards()
    val outliers = cardset.get_outlier_scores()
    val H = ListBuffer[Hypothesis]()
    for(c<-outliers) {
      println(c._1 + "," + c._2)
      H.addOne(Hypothesis(c._1, c._1.mean(c._1.sample),c._1.stdev(c._1.sample), c._2 < std_dev_range*(-1) || c._2 > std_dev_range))
    }
    HypothesisSet(H.toList)
  }

}
