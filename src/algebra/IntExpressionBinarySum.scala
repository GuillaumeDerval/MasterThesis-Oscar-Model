package algebra

import scala.collection.mutable.HashSet

/**
 * Expression left+right
 * @param left left-hand of the sum
 * @param right right-hand of the sum
 */
class IntExpressionBinarySum(val left: IntExpression, val right: IntExpression) extends IntExpression {
  override def evaluate(): Int = left.evaluate() + right.evaluate()
  override def min: Int = left.min + right.min
  override def max: Int = left.max + right.max
  override def iterator: Iterator[Int] = {
    val s = new HashSet[Int]()
    for(i <- left)
      for(j <- right)
        s += i+j
    s.iterator
  }
}
