package algebra

import misc.VariableNotBoundException

/**
 * and_i a_i = output
 */
case class And(a: Array[BoolExpression]) extends BoolExpression {
  /**
   * Evaluate this expression. All variables referenced have to be bound.
   * @throws VariableNotBoundException when a variable is not bound
   * @return the value of this expression
   */
  override def evaluateBool(): Boolean = a.foldLeft(true)((acc, v) => acc && v.evaluateBool())
}