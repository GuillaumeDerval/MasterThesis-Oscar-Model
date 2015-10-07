package models.uninstantiated

import misc.UnionFindStorage
import models.{Model, ModelDeclaration}
import vars.IntView
import vars.domainstorage.DomainStorage
import vars.domainstorage.int.IntDomainStorage

import scala.collection.mutable.ArrayBuffer

/**
 * A non-instantiated model, containing DomainStorage as implementations.
 * @param declaration: the ModelDeclaration associated with this model
 */
class BaseModel(val declaration: ModelDeclaration) extends UninstantiatedModel {
  override val parent: Option[Model] = None
  override val intDomains: UnionFindStorage[IntDomainStorage] = UnionFindStorage[IntDomainStorage]()
  override val intViews: ArrayBuffer[IntView] = new ArrayBuffer[IntView]()
}