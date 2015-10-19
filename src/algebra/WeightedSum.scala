package algebra

/**
 * Weighted sum of X with W (scalar product between these vectors)
 */
case class WeightedSum(val X: Array[IntExpression], val W: Array[Int]) extends IntExpression {
  override def evaluate(): Int = X.zip(W).foldLeft(0)((acc: Int, e: (IntExpression, Int)) => acc + (e._1.evaluate()*e._2))
  override def min: Int = X.zip(W).foldLeft(0)((acc: Int, e: (IntExpression, Int)) => acc + e._1.min*e._2)
  override def max: Int = X.zip(W).foldLeft(0)((acc: Int, e: (IntExpression, Int)) => acc + e._1.max*e._2)
  override def iterator: Iterator[Int] = Range(min, max+1).iterator
}
