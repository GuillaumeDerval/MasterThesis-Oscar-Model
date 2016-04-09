package algebra

import scala.collection.mutable.HashSet

/**
 * Min of an array
 */
case class Min(val a: Array[IntExpression]) extends IntExpression {
  override def evaluate(): Int = a.foldLeft(Integer.MAX_VALUE)((aa: Int, ba: IntExpression) => aa min ba.evaluate())
  override def min: Int = a.foldLeft(Integer.MAX_VALUE)((aa: Int, ba: IntExpression) => aa min ba.min)
  override def max: Int = a.foldLeft(Integer.MIN_VALUE)((aa: Int, ba: IntExpression) => aa max ba.min)
  override def values(): Iterable[Int] = {
    //TODO: we can make it better easily
    val s = new HashSet[Int]()
    for(i <- a)
      s ++= i.values
    s
  }

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[IntExpression] = a

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = new Min(a.map(func))
}
