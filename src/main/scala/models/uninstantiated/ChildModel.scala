package models.uninstantiated

import constraints.Constraint
import misc.{ModelVarStorage}
import models.{Model, ModelDeclaration, OptimisationMethod}
import vars.IntVar
import vars.domainstorage.int.IntDomainStorage

/**
 * A model that inherits from another one
 */
class ChildModel(p: UninstantiatedModel) extends UninstantiatedModel {
  override val declaration: ModelDeclaration = p.declaration
  override var intRepresentatives: ModelVarStorage[IntVar, IntDomainStorage] = p.intRepresentatives
  override var optimisationMethod: OptimisationMethod = p.optimisationMethod
  override var constraints: List[Constraint] = p.constraints
}
