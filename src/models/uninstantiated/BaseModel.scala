package models.uninstantiated

import misc.UnionFindStorage
import models.{NoOptimisation, OptimisationMethod, Model, ModelDeclaration}
import vars.domainstorage.int.IntDomainStorage

/**
  * A non-instantiated model, containing DomainStorage as implementations.
  * @param declaration: the ModelDeclaration associated with this model
  */
class BaseModel(val declaration: ModelDeclaration) extends UninstantiatedModel {
  override val parent: Option[Model] = None
  override val intRepresentatives: UnionFindStorage[IntDomainStorage] = UnionFindStorage[IntDomainStorage]()
  override var optimisationMethod: OptimisationMethod = new NoOptimisation
}