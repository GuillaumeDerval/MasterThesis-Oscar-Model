package models.uninstantiated

import constraints.Constraint
import misc.ModelVarStorage
import models.{ModelDeclaration, NoOptimisation, OptimisationMethod}
import vars.IntVar
import vars.domainstorage.int.IntDomainStorage

/**
  * A non-instantiated model, containing DomainStorage as implementations.
  * @param declaration: the ModelDeclaration associated with this model
  */
class BaseModel(val declaration: ModelDeclaration) extends UninstantiatedModel {
  override var intRepresentatives: ModelVarStorage[IntVar, IntDomainStorage] = ModelVarStorage[IntVar, IntDomainStorage]()
  override var optimisationMethod: OptimisationMethod = new NoOptimisation
  override var constraints: List[Constraint] = Nil
}