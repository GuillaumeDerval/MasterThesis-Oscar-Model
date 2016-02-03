package models.uninstantiated

import constraints.Constraint
import misc.UnionFindStorage
import models.{OptimisationMethod, Model, ModelDeclaration}
import vars.domainstorage.DomainStorage
import vars.domainstorage.int.IntDomainStorage

import scala.collection.mutable

/**
 * A model that inherits from another one
 */
class ChildModel(p: UninstantiatedModel) extends UninstantiatedModel {
  override val parent: Option[Model] = Some(p)
  override val declaration: ModelDeclaration = p.declaration
  override val intRepresentatives: UnionFindStorage[IntDomainStorage] = UnionFindStorage[IntDomainStorage, IntDomainStorage](p.intRepresentatives, _.copy())
  override var optimisationMethod: OptimisationMethod = p.optimisationMethod
  override val constraints = p.constraints.clone()
}
