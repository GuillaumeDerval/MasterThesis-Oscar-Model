package algebra

import misc.VariableNotBoundException

/**
 * a ^ b
 */
class BoolExpressionXor(a: BoolExpression, b: BoolExpression) extends BoolExpression {
  /**
   * Evaluate this expression. All variables referenced have to be bound.
   * @throws VariableNotBoundException when a variable is not bound
   * @return the value of this expression
   */
  override def evaluateBool(): Boolean = a.evaluateBool() ^ b.evaluateBool()
}