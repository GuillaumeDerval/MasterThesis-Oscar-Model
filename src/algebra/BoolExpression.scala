package algebra

import constraints.{ExpressionConstraint, Constraint}
import misc.{VariableNotBoundException, EmptyDomainException}
import models.ModelDeclaration
import vars.BoolVar

/**
 * Represents a Boolean expression (an IntExpression that returns a boolean, 0 or 1)
 */
trait BoolExpression extends IntExpression {
  /**
   * Return a lower bound for this expression
   */
  def min: Int = 0

  /**
   * Return a higher bound for this expression
   */
  def max: Int = 1

  /**
   * Evaluate this expression. All variables referenced have to be bound.
   * @throws VariableNotBoundException when a variable is not bound
   * @return the value of this expression
   */
  def evaluate(): Int = if(evaluateBool()) 1 else 0
  def evaluateBool(): Boolean

  /**
   * Give a variable that is equal to this expression. May post appropriate constraints.
   * @param modelDeclaration the ModelDeclaration object in which new variable/constraint will be created
   * @throws EmptyDomainException when the new IntVar has an empty domain
   * @return an IntVar
   */
  override def reify()(implicit modelDeclaration: ModelDeclaration): BoolVar = {
    val z = BoolVar(min == 0, max == 1)(modelDeclaration)
    modelDeclaration.post(new BoolExpressionEq(this, z))
    z
  }

  /**
   * Get an iterator to all the values that this expression can take
   */
  override def iterator: Iterator[Int] = Set(0, 1).iterator

  def toConstraint: Constraint = new ExpressionConstraint(this)
}

object BoolExpression {
  /**
   * Convert a BoolExpression to an equivalent constraint
   */
  implicit def toConstraint(boolExpression: BoolExpression): Constraint = boolExpression.toConstraint
}