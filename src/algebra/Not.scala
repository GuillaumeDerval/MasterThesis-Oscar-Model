package algebra

import misc.VariableNotBoundException

/**
 * !x
 */
class Not(x: BoolExpression) extends BoolExpression {
  /**
   * Evaluate this expression. All variables referenced have to be bound.
   * @throws VariableNotBoundException when a variable is not bound
   * @return the value of this expression
   */
  override def evaluateBool(): Boolean = !x.evaluateBool()
}