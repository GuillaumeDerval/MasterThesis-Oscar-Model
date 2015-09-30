package models.uninstantiated

import misc.UnionFindStorage
import models.{Model, ModelDeclaration}
import vars.domainstorage.DomainStorage
import vars.domainstorage.int.IntDomainStorage

/**
 * A non-instantiated model, containing DomainStorage as implementations.
 * @param declaration: the ModelDeclaration associated with this model
 */
class BaseModel(val declaration: ModelDeclaration) extends UninstantiatedModel {
  override val parent: Option[Model] = None
  override val intDomains: UnionFindStorage[IntDomainStorage] = UnionFindStorage[IntDomainStorage]()
}