package algebra

/**
 * Return N, the count of variables in X that are equals to Y
 * @param X
 * @param Y
 */
case class Count(X: Array[IntExpression], Y: IntExpression) extends IntExpression{
  override def evaluate(): Int = {
    val vy = Y.evaluate()
    X.foldLeft(0)((acc, v) => if(v.evaluate() == vy) acc+1 else acc)
  }
  override def max: Int = X.length
  override def min: Int = 0
  override def iterator: Iterator[Int] = Range(min, max+1).iterator
}