package vars

import algebra.BoolExpression
import constraints.{BoolExpressionFalse, BoolExpressionTrue, Constraint}
import models.ModelDeclaration
import vars.domainstorage.int.IntDomainStorage

class BoolVar(model_decl: ModelDeclaration, storage: IntDomainStorage) extends IntVar(model_decl, storage) with BoolVarLike with BoolExpression
{
  def constraintTrue(): Constraint = new BoolExpressionTrue(this)
  def constraintFalse(): Constraint = new BoolExpressionFalse(this)

  override def max: Int = getRepresentative.max
  override def min: Int = getRepresentative.min
}

object BoolVar {
  def apply(containsFalse: Boolean, containsTrue: Boolean)(implicit model_decl: ModelDeclaration) = {
    new BoolVar(model_decl, IntDomainStorage(if (containsFalse) 0 else 1, if (containsFalse) 1 else 0))
  }
}