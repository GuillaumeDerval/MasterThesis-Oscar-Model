package vars

import algebra.{BoolExpressionNot, BoolExpression}
import constraints.{ExpressionConstraint, Constraint}
import models.ModelDeclaration
import vars.domainstorage.int.IntDomainStorage

class BoolVar(model_decl: ModelDeclaration, storage: IntDomainStorage) extends IntVar(model_decl, storage) with BoolVarLike with BoolExpression
{
  /**
   * @return a constraint that imposes this variable is true
   */
  def constraintTrue(): Constraint = new ExpressionConstraint(this)

  /**
   * @return a constraint that imposes this variable is false
   */
  def constraintFalse(): Constraint = new ExpressionConstraint(new BoolExpressionNot(this))

  // Scala imposes to choose between the implementation in IntVar and the one in BoolExpression;
  // we choose the one in IntVar
  override def max: Int = this.asInstanceOf[IntVar].max
  override def min: Int = this.asInstanceOf[IntVar].min
  override def evaluate(): Int = this.asInstanceOf[IntVar].evaluate()
  override def evaluateBool(): Boolean = evaluate() == 1
}

object BoolVar {
  def apply(containsFalse: Boolean, containsTrue: Boolean)(implicit model_decl: ModelDeclaration) = {
    new BoolVar(model_decl, IntDomainStorage(if (containsFalse) 0 else 1, if (containsFalse) 1 else 0))
  }
}