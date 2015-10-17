package algebra

import scala.collection.mutable.HashSet

/**
 * Max of an array
 */
class Max(val a: Array[IntExpression]) extends IntExpression {
  override def evaluate(): Int = a.foldLeft(Integer.MIN_VALUE)((aa: Int, ba: IntExpression) => aa max ba.evaluate())
  override def min: Int = a.foldLeft(Integer.MAX_VALUE)((aa: Int, ba: IntExpression) => aa min ba.max)
  override def max: Int = a.foldLeft(Integer.MIN_VALUE)((aa: Int, ba: IntExpression) => aa max ba.max)
  override def iterator: Iterator[Int] = {
    //TODO: we can make it better easily
    val s = new HashSet[Int]()
    for(i <- a)
      s ++= i
    s.iterator
  }
}
