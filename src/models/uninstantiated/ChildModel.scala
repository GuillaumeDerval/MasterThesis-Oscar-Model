package models.uninstantiated

import misc.UnionFindStorage
import models.{Model, ModelDeclaration}
import vars.domainstorage.DomainStorage
import vars.domainstorage.int.IntDomainStorage

/**
 * A model that inherits from another one
 */
class ChildModel(p: UninstantiatedModel) extends UninstantiatedModel {
  override val parent: Option[Model] = Some(p)
  override val declaration: ModelDeclaration = p.declaration
  override val intDomains: UnionFindStorage[IntDomainStorage] = UnionFindStorage[IntDomainStorage, IntDomainStorage](p.intDomains, _.copy())
}
