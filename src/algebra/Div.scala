package algebra

import scala.collection.mutable.HashSet

/**
 * Expression left/right (euclidian division)
 * @param left The numerator
 * @param right The denoinator
 */
case class Div(val left: IntExpression, val right: IntExpression) extends IntExpression {
  override def evaluate(): Int = left.evaluate() / right.evaluate()
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
    for(i <- left)
      for(j <- right)
        s += i/j
    s.iterator
  }
}
