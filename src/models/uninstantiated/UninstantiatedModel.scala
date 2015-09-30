package models.uninstantiated

import models.Model
import vars.domainstorage.DomainStorage
import vars.domainstorage.int.IntDomainStorage

/**
 * Represent any non-instantiated model. An UninstantiatedModel is exportable and importable
 */
abstract class UninstantiatedModel extends Model {
  override type IntVarImplementation = IntDomainStorage
}