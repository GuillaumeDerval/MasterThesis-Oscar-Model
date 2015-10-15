package algebra

import scala.collection.mutable.HashSet

/**
 * Expression for element constraint
 * @param table The table in which we will retrieve the element ``key``
 * @param key The key to retrieve in ``table``
 */
class IntExpressionElementCst(val table: Array[Int], val key: IntExpression) extends IntExpression {
  override def evaluate(): Int = table(key.evaluate())
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
    for(i <- key)
      s += table(i)
    s.iterator
  }
}
