package models.instantiated

import misc.ModelVarStorage
import models.uninstantiated.UninstantiatedModel
import models.{Model, ModelDeclaration, OptimisationMethod}
import vars.IntVar
import vars.domainstorage.DomainStorage
import vars.domainstorage.int._

/**
 * A trait representing all instantiated models
 */
abstract class InstantiatedModel(p: UninstantiatedModel) extends Model {
  override val declaration: ModelDeclaration = p.declaration
  override var intRepresentatives: ModelVarStorage[IntVar, IntVarImplementation] = ModelVarStorage[IntVar, IntVarImplementation, IntDomainStorage](p.intRepresentatives, instantiateDomainStorage)
  override var optimisationMethod: OptimisationMethod = p.optimisationMethod

  for(c <- p.constraints)
    post(c)

  protected def instantiateDomainStorage(v: DomainStorage): IntVarImplementation = {
    //Cannot do pattern matching here as Implementation is not fully defined
    v match {
      case adaptable: AdaptableIntDomainStorage => instantiateAdaptableIntDomainStorage(adaptable)
      case interval: IntervalDomainStorage => instantiateIntervalDomainStorage(interval)
      case set: SetDomainStorage => instantiateSetDomainStorage(set)
      case singleton: SingletonDomainStorage => instantiateSingletonDomainStorage(singleton)
      case _ => sys.error("Unknown DomainStorage type in InstantiatedModel.instantiateDomainStorage")
    }
  }

  protected def instantiateAdaptableIntDomainStorage(adaptable: AdaptableIntDomainStorage): IntVarImplementation

  protected def instantiateIntervalDomainStorage(interval: IntervalDomainStorage): IntVarImplementation

  protected def instantiateSetDomainStorage(set: SetDomainStorage): IntVarImplementation

  protected def instantiateSingletonDomainStorage(singleton: SingletonDomainStorage): IntVarImplementation
}