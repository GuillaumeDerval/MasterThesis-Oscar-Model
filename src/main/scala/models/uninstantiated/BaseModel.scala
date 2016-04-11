package models.uninstantiated

import misc.{ModelVarStorage, UnionFindStorage}
import models.{Model, ModelDeclaration, NoOptimisation, OptimisationMethod}
import vars.IntVar
import vars.domainstorage.int.IntDomainStorage

/**
  * A non-instantiated model, containing DomainStorage as implementations.
  * @param declaration: the ModelDeclaration associated with this model
  */
class BaseModel(val declaration: ModelDeclaration) extends UninstantiatedModel {
  override val parent: Option[Model] = None
  val intRepresentatives: ModelVarStorage[IntVar, IntDomainStorage] = ModelVarStorage[IntVar, IntDomainStorage]()
  override var optimisationMethod: OptimisationMethod = new NoOptimisation
}