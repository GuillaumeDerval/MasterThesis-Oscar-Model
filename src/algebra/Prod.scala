package algebra

import scala.collection.mutable.HashSet

/**
 * Expression left*right
 * @param left left-hand of the product
 * @param right right-hand of the product
 */
class Prod(val left: IntExpression, val right: IntExpression) extends IntExpression {
  override def evaluate(): Int = left.evaluate() * right.evaluate()
  override def min: Int = Math.min(Math.min(left.min*right.min, left.min*right.max), Math.min(left.max*right.min, left.max*right.max))
  override def max: Int = Math.max(Math.max(left.min*right.min, left.min*right.max), Math.max(left.max*right.min, left.max*right.max))
  override def iterator: Iterator[Int] = {
    val s = new HashSet[Int]()
    for(i <- left)
      for(j <- right)
        s += i*j
    s.iterator
  }
}