package algebra

import misc.VariableNotBoundException

/**
 * or_i a_i = output
 */
class BoolExpressionOr(a: Array[BoolExpression]) extends BoolExpression {
  /**
   * Evaluate this expression. All variables referenced have to be bound.
   * @throws VariableNotBoundException when a variable is not bound
   * @return the value of this expression
   */
  override def evaluateBool(): Boolean = a.foldLeft(false)((acc, e) => acc || e.evaluateBool())
}