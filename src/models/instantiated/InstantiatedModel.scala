package models.instantiated

import misc.UnionFindStorage
import models.uninstantiated.UninstantiatedModel
import models.{OptimisationMethod, Model, ModelDeclaration}
import vars.domainstorage.DomainStorage
import vars.domainstorage.int._

/**
 * A trait representing all instantiated models
 */
abstract class InstantiatedModel(p: UninstantiatedModel) extends Model {
  override val parent: Option[Model] = Some(p)
  override val declaration: ModelDeclaration = p.declaration
  override val intRepresentatives: UnionFindStorage[IntVarImplementation] = UnionFindStorage[IntVarImplementation, IntDomainStorage](p.intRepresentatives, instantiateDomainStorage)
  override var optimisationMethod: OptimisationMethod = p.optimisationMethod

  for(c <- p.constraints)
    post(c)

  protected def instantiateDomainStorage(v: DomainStorage): IntVarImplementation = {
    //Cannot do pattern matching here as Implementation is not fully defined
    if (v.isInstanceOf[AdaptableIntDomainStorage])
      instantiateAdaptableIntDomainStorage(v.asInstanceOf[AdaptableIntDomainStorage])
    else if (v.isInstanceOf[IntervalDomainStorage])
      instantiateIntervalDomainStorage(v.asInstanceOf[IntervalDomainStorage])
    else if (v.isInstanceOf[SetDomainStorage])
      instantiateSetDomainStorage(v.asInstanceOf[SetDomainStorage])
    else if (v.isInstanceOf[SingletonDomainStorage])
      instantiateSingletonDomainStorage(v.asInstanceOf[SingletonDomainStorage])
    else
      sys.error("Unknown DomainStorage type in InstantiatedModel.instantiateDomainStorage")
  }

  protected def instantiateAdaptableIntDomainStorage(adaptable: AdaptableIntDomainStorage): IntVarImplementation

  protected def instantiateIntervalDomainStorage(interval: IntervalDomainStorage): IntVarImplementation

  protected def instantiateSetDomainStorage(set: SetDomainStorage): IntVarImplementation

  protected def instantiateSingletonDomainStorage(singleton: SingletonDomainStorage): IntVarImplementation
}