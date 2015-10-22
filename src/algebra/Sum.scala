package algebra

import scala.collection.mutable.HashSet

/**
 * Sum of an array of expression
 */
case class Sum(val v: Array[IntExpression]) extends IntExpression {
  override def evaluate(): Int = v.foldLeft(0)((acc: Int, e: IntExpression) => acc + e.evaluate())
  override def min: Int = v.foldLeft(0)((acc: Int, e: IntExpression) => acc + e.min)
  override def max: Int = v.foldLeft(0)((acc: Int, e: IntExpression) => acc + e.max)
  override def values(): Iterable[Int] = Range(min, max+1)

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[IntExpression] = v

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = new Sum(v.map(func))
}
