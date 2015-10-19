package algebra

import misc.VariableNotBoundException

/**
 * a \in b
 */
case class InSet(a: IntExpression, b: Set[Int]) extends BoolExpression {
  /**
   * Evaluate this expression. All variables referenced have to be bound.
   * @throws VariableNotBoundException when a variable is not bound
   * @return the value of this expression
   */
  override def evaluateBool(): Boolean = b.contains(a.evaluate())
}