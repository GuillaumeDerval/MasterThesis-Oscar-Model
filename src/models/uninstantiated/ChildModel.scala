package models.uninstantiated

import misc.UnionFindStorage
import models.{Model, ModelDeclaration}
import vars.domainstorage.DomainStorage

/**
 * A model that inherits from another one
 */
class ChildModel(p: UninstantiatedModel) extends UninstantiatedModel {
  override val parent: Option[Model] = Some(p)
  override val declaration: ModelDeclaration = p.declaration
  override val domains: UnionFindStorage[DomainStorage] = UnionFindStorage[DomainStorage, DomainStorage](p.domains, _.copy())
}
