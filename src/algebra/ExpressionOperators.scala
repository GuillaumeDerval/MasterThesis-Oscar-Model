package algebra

/**
 * Basic operations on expressions
 */
object ExpressionOperators {
  import IntExpression._
  import BoolExpression._

  def min(a: IntExpression*): IntExpression = new IntExpressionMin(a.toArray)
  def min(a: Array[IntExpression]): IntExpression = new IntExpressionMin(a)
  def max(a: IntExpression*): IntExpression = new IntExpressionMax(a.toArray)
  def max(a: Array[IntExpression]): IntExpression = new IntExpressionMax(a)
  def sum(a: IntExpression*): IntExpression = new IntExpressionSum(a.toArray)
  def sum(a: Array[IntExpression]): IntExpression = new IntExpressionSum(a)
}
