package algebra

import vars.IntVar

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

object Sum {
  def apply(v: Iterable[IntExpression]): Sum = Sum(v.toArray)

  def apply[A](indices: Iterable[A])(f: A => IntExpression): Sum = Sum(indices map f)

  def apply[A, B](indices1: Iterable[A], indices2: Iterable[B])(f: (A, B) => IntExpression): Sum = Sum(for (i <- indices1; j <- indices2) yield f(i, j))

  def apply(n1: Int, n2: Int)(f: (Int, Int) => IntExpression): Sum = Sum(0 until n1, 0 until n2)(f)
}