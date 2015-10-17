package algebra

import scala.collection.mutable.HashSet

/**
 * Min of an array
 */
class Min(val a: Array[IntExpression]) extends IntExpression {
  override def evaluate(): Int = a.foldLeft(Integer.MAX_VALUE)((aa: Int, ba: IntExpression) => aa min ba.evaluate())
  override def min: Int = a.foldLeft(Integer.MAX_VALUE)((aa: Int, ba: IntExpression) => aa min ba.min)
  override def max: Int = a.foldLeft(Integer.MIN_VALUE)((aa: Int, ba: IntExpression) => aa max ba.min)
  override def iterator: Iterator[Int] = {
    //TODO: we can make it better easily
    val s = new HashSet[Int]()
    for(i <- a)
      s ++= i
    s.iterator
  }
}
