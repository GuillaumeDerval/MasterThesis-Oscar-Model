package models

import misc.UnionFindStorage
import vars.domainstorage.DomainStorage

/**
 * A model that inherits from another one
 */
class ChildrenModel(p: UninstantiatedModel) extends UninstantiatedModel
{
  override val parent: Option[Model] = Some(p)
  override val declaration: ModelDeclaration = p.declaration
  override val domains: UnionFindStorage[DomainStorage] = UnionFindStorage[DomainStorage,DomainStorage](p.domains, _.copy())
}
