package DistroManager

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Author: Casey Ford
 * July 2021
 */
case class Card(cardname : String) {

  def get_distro() : (Double,Double) = {
    val vals = parse_distrofile()

    val mu = mean(vals)
    val sig = stdev(vals)
    (mu,sig)
  }

  def parse_distrofile() : List[Double] = {
    val file = DistributionManager.get_distro_filename(cardname)
    val lines = Source.fromFile(file).getLines()
    val ret : ListBuffer[Double] = ListBuffer[Double]()
    for(l<-lines) { // <INT,DOUBLE,INT,INT>
      val args = l.split(",")
      ret.addOne(args(1).toDouble)
    }
    ret.toList
  }

  def tell_distrofile(kingdomsize : Int, power : Double) : Unit = {
    val distrofile = DistributionManager.get_distro_filename(cardname)

    val bw = new BufferedWriter(new FileWriter(new File(distrofile), true))
    bw.write(kingdomsize + "," + power + "\n") // <INT,DOUBLE>
    bw.close()
  }

  def mean(values : List[Double]) : Double = {
    values.sum/values.length
  }

  def stdev(values : List[Double]) : Double = {
    val mu : Double = mean(values)
    val n : Int = values.length

    val sumdiffsq : Double = {
      var inside : Double = 0.0
      for (i <- values) {
        inside += math.pow((i - mu),2)
      }
      inside
    }

    math.sqrt((sumdiffsq)/(n - 1))

  }

  override def toString: String = {
    cardname
  }
}
