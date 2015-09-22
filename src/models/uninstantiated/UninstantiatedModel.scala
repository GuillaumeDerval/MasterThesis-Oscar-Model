package models.uninstantiated

import models.Model
import vars.domainstorage.DomainStorage

/**
 * Represent any non-instantiated model. An UninstantiatedModel is exportable and importable
 */
abstract class UninstantiatedModel extends Model
{
  override type Implementation = DomainStorage
}