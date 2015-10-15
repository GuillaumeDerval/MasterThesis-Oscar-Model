package algebra

import scala.collection.mutable.HashSet

/**
 * Expression max(left, right)
 * @param left first expression
 * @param right second expression
 */
class IntExpressionMax(val left: IntExpression, val right: IntExpression) extends IntExpression {
  override def evaluate(): Int = Math.max(left.evaluate(),right.evaluate())
  override def min: Int = Math.max(left.min, right.min)
  override def max: Int = Math.max(left.max, right.max)
  override def iterator: Iterator[Int] = {
    //TODO: we can make it better easily
    val s = new HashSet[Int]()
    for(i <- left)
      for(j <- right)
        s += Math.max(i,j)
    s.iterator
  }
}
