package models.uninstantiated

import constraints.Constraint
import models.Model
import vars.domainstorage.int.IntDomainStorage

/**
 * Represent any non-instantiated model. An UninstantiatedModel is exportable and importable
 */
abstract class UninstantiatedModel extends Model {
  override type IntVarImplementation = IntDomainStorage
  var constraints: List[Constraint]

  /**
   * Post a new constraint
   * @param constraint
   */
  def post(constraint: Constraint): Unit = constraints = constraint :: constraints

  override protected def optimisationMethodUpdated(): Unit = {}
}