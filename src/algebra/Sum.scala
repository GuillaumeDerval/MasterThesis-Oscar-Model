package algebra

import scala.collection.mutable.HashSet

/**
 * Sum of an array of expression
 */
class Sum(val v: Array[IntExpression]) extends IntExpression {
  override def evaluate(): Int = v.foldLeft(0)((acc: Int, e: IntExpression) => acc + e.evaluate())
  override def min: Int = v.foldLeft(0)((acc: Int, e: IntExpression) => acc + e.min)
  override def max: Int = v.foldLeft(0)((acc: Int, e: IntExpression) => acc + e.max)
  override def iterator: Iterator[Int] = Range(min, max+1).iterator
}
