package DistroManager

import scala.io.Source

case class Card(cardname : String, filename : String) {

  def get_distro(values : List[Double]) : (Double,Double) = {



    (0.0,0.0)
  }

  def parse_distrofile() : List[Double] = {

    val file = DistributionManager.get_distro_filename(cardname)
    val lines = Source.fromFile(file).getLines()
    for(l<-lines) {

    }
    List()
  }

  def tell_distrofile() : Unit = {

  }

  def mean(values : List[Double]) : Double = {
    values.sum/values.length
  }

}
