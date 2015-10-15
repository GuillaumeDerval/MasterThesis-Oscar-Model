package algebra

import constraints.IntExpressionEquivalence
import misc.EmptyDomainException
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
   * Give a variable that is equal to this expression. May post appropriate constraints.
   * @param modelDeclaration the ModelDeclaration object in which new variable/constraint will be created
   * @throws EmptyDomainException when the new IntVar has an empty domain
   * @return an IntVar
   */
  override def reify(modelDeclaration: ModelDeclaration): BoolVar = {
    val z = BoolVar(min == 0, max == 1)(modelDeclaration)
    modelDeclaration.post(new IntExpressionEquivalence(this, z))
    z
  }
}
