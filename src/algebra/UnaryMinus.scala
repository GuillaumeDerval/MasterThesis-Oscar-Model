package algebra

/**
 * Expression of -value
 * @param value the value of the constant
 */
case class UnaryMinus(val value: IntExpression) extends IntExpression {
  override def evaluate(): Int = -value.evaluate()
  override def min: Int = -value.max
  override def max: Int = -value.min
  override def iterator: Iterator[Int] = value.iterator.map(-_)
}
