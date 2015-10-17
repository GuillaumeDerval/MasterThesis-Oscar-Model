package algebra

import misc.VariableNotBoundException

/**
 * a <= b
 */
class LrEq(a: IntExpression, b: IntExpression) extends BoolExpression {
  /**
   * Evaluate this expression. All variables referenced have to be bound.
   * @throws VariableNotBoundException when a variable is not bound
   * @return the value of this expression
   */
  override def evaluateBool(): Boolean = a.evaluate() <= b.evaluate()
}