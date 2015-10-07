package models.uninstantiated

import misc.UnionFindStorage
import models.{Model, ModelDeclaration}
import vars.IntView
import vars.domainstorage.DomainStorage
import vars.domainstorage.int.IntDomainStorage

import scala.collection.mutable.ArrayBuffer

/**
 * A model that inherits from another one
 */
class ChildModel(p: UninstantiatedModel) extends UninstantiatedModel {
  override val parent: Option[Model] = Some(p)
  override val declaration: ModelDeclaration = p.declaration
  override val intDomains: UnionFindStorage[IntDomainStorage] = UnionFindStorage[IntDomainStorage, IntDomainStorage](p.intDomains, _.copy())
  override val intViews: ArrayBuffer[IntView] = new ArrayBuffer[IntView](p.intViews.length)
  for(i: IntView <- p.intViews)
    intViews += i.copy()
}
