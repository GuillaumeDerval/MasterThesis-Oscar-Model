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

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[IntExpression] = a

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = {
    new And(a.map(func(_).asInstanceOf[BoolExpression]))
  }
}