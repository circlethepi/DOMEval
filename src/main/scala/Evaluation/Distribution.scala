package Evaluation

trait Distribution {
  var sample: List[Double] = List[Double]()


  def add_to_sample(item: Double): List[Double] = {
    sample = sample.concat(List(item))
    sample
  }
  def add_to_sample(items : List[Double]): List[Double] = {
    sample = sample.concat(items)
    sample
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

    math.sqrt(sumdiffsq/(n - 1))
  }
}
