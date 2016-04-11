package models.uninstantiated

import misc.UnionFindStorage
import models.{Model, ModelDeclaration, OptimisationMethod}
import vars.domainstorage.int.IntDomainStorage

/**
 * A model that inherits from another one
 */
class ChildModel(p: UninstantiatedModel) extends UninstantiatedModel {
  override val parent: Option[Model] = Some(p)
  override val declaration: ModelDeclaration = p.declaration
  override val intRepresentatives: UnionFindStorage[IntDomainStorage] = UnionFindStorage[IntDomainStorage, IntDomainStorage](p.intRepresentatives, a => a)
  override var optimisationMethod: OptimisationMethod = p.optimisationMethod
  override val constraints = p.constraints.clone()
}
