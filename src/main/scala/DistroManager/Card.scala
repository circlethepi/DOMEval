package DistroManager

import Evaluation.Distribution

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Author: Casey Ford
 * July 2021
 */
case class Card(cardname : String) extends Distribution {

  def parse_distrofile() : List[Double] = {                       //POPULATION DISTRIBUTION
    val file = DistributionManager.get_distro_filename(cardname)
    val lines = Source.fromFile(file).getLines()

    for(l<-lines) { // <INT,DOUBLE,INT,INT>
      val args = l.split(",")
      add_to_sample(args(0).toDouble)
    }

    sample
  }

  def tell_distrofile(kingdomsize : Int, power : Double) : Unit = {     //MANAGING POP
    val distrofile = DistributionManager.get_distro_filename(cardname)

    val bw = new BufferedWriter(new FileWriter(new File(distrofile), true))
    bw.write(kingdomsize + "," + power + "\n") // <INT,DOUBLE>
    bw.close()
  }

  override def toString: String = {
    cardname
  }
}
