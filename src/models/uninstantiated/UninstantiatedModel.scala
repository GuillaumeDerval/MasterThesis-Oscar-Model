package models.uninstantiated

import models.Model
import vars.IntView
import vars.domainstorage.DomainStorage
import vars.domainstorage.int.IntDomainStorage

import scala.collection.mutable.ArrayBuffer

/**
 * Represent any non-instantiated model. An UninstantiatedModel is exportable and importable
 */
abstract class UninstantiatedModel extends Model {
  override type IntVarImplementation = IntDomainStorage
}