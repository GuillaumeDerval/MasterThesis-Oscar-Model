package algebra

import scala.collection.mutable.HashSet

/**
 * Expression for pow(base,exponent)
 * @param base the base of the exponential
 * @param exponent the exponent
 */
class Exponent(val base: IntExpression, val exponent: IntExpression) extends IntExpression {
  override def evaluate(): Int = Math.pow(base.evaluate(), exponent.evaluate()).toInt
  override def min: Int = {
    //TODO: we can make it better easily
    iterator.min
  }
  override def max: Int = {
    //TODO: we can make it better easily
    iterator.max
  }
  override def iterator: Iterator[Int] = {
    val s = new HashSet[Int]()
    for(i <- base)
      for(j <- exponent)
        s += Math.pow(i,j).toInt
    s.iterator
  }
}
