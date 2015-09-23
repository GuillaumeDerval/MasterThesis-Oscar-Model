package models.instantiated

import misc.UnionFindStorage
import models.uninstantiated.UninstantiatedModel
import models.{Model, ModelDeclaration}
import vars.domainstorage.DomainStorage
import vars.domainstorage.int.{AdaptableIntDomainStorage, IntervalDomainStorage, SetDomainStorage, SingletonDomainStorage}

/**
 * A trait representing all instantiated models
 */
abstract class InstantiatedModel(p: UninstantiatedModel) extends Model {
  override val parent: Option[Model] = Some(p)
  override val declaration: ModelDeclaration = p.declaration
  override val domains: UnionFindStorage[Implementation] = UnionFindStorage[Implementation, DomainStorage](p.domains, instantiateDomainStorage)

  protected def instantiateDomainStorage(v: DomainStorage): Implementation = {
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

  protected def instantiateAdaptableIntDomainStorage(adaptable: AdaptableIntDomainStorage): Implementation

  protected def instantiateIntervalDomainStorage(interval: IntervalDomainStorage): Implementation

  protected def instantiateSetDomainStorage(set: SetDomainStorage): Implementation

  protected def instantiateSingletonDomainStorage(singleton: SingletonDomainStorage): Implementation
}