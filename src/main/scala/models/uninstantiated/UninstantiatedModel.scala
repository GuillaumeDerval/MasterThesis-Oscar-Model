package models.uninstantiated

import constraints.Constraint
import models.Model
import vars.domainstorage.int.IntDomainStorage

import scala.collection.mutable

/**
 * Represent any non-instantiated model. An UninstantiatedModel is exportable and importable
 */
abstract class UninstantiatedModel extends Model {
  override type IntVarImplementation = IntDomainStorage
  val constraints = new mutable.MutableList[Constraint]

  /**
   * Post a new constraint
   * @param constraint
   */
  def post(constraint: Constraint): Unit = constraints += constraint

  override protected def optimisationMethodUpdated(): Unit = {}
}