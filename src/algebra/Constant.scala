package algebra

/**
 * Expression of a constant
 * @param value the value of the constant
 */
case class Constant(val value: Int) extends IntExpression {
  override def evaluate(): Int = value
  override def min: Int = value
  override def max: Int = value
  override def iterator: Iterator[Int] = List(value).iterator
}