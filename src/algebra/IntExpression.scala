package algebra

import constraints.IntExpressionEquivalence
import misc.{EmptyDomainException, VariableNotBoundException}
import models.ModelDeclaration
import vars.IntVar

/**
 * An expression that represents an Integer
 */
trait IntExpression extends Iterable[Int] {
  /**
   * Evaluate this expression. All variables referenced have to be bound.
   * @throws VariableNotBoundException when a variable is not bound
   * @return the value of this expression
   */
  def evaluate(): Int

  /**
   * Return a lower bound for this expression
   */
  def min: Int

  /**
   * Return a higher bound for this expression
   */
  def max: Int

  /**
   * Get an iterator to all the values that this expression can take
   */
  def iterator: Iterator[Int]
  override def foreach[@specialized(Int) U](f: Int => U): Unit = iterator.foreach(f)

  /**
   * Give a variable that is equal to this expression. May post appropriate constraints.
   * @param modelDeclaration the ModelDeclaration object in which new variable/constraint will be created
   * @throws EmptyDomainException when the new IntVar has an empty domain
   * @return an IntVar
   */
  def reify(modelDeclaration: ModelDeclaration): IntVar = {
    val z = IntVar(min, max)(modelDeclaration)
    modelDeclaration.post(new IntExpressionEquivalence(this, z))
    z
  }
}