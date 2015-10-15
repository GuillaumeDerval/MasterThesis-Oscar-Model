package algebra

import scala.collection.mutable.HashSet

/**
 * Expression min(left, right)
 * @param left first expression
 * @param right second expression
 */
class IntExpressionMin(val left: IntExpression, val right: IntExpression) extends IntExpression {
  override def evaluate(): Int = Math.min(left.evaluate(),right.evaluate())
  override def min: Int = Math.min(left.min, right.min)
  override def max: Int = Math.min(left.max, right.max)
  override def iterator: Iterator[Int] = {
    //TODO: we can make it better easily
    val s = new HashSet[Int]()
    for(i <- left)
      for(j <- right)
        s += Math.min(i,j)
    s.iterator
  }
}
