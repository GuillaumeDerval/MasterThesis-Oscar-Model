package models.instantiated

import misc.UnionFindStorage
import models.uninstantiated.UninstantiatedModel
import models.{Model, ModelDeclaration}
import vars.IntView
import vars.domainstorage.DomainStorage
import vars.domainstorage.int._

import scala.collection.mutable.ArrayBuffer

/**
 * A trait representing all instantiated models
 */
abstract class InstantiatedModel(p: UninstantiatedModel) extends Model {
  override val parent: Option[Model] = Some(p)
  override val declaration: ModelDeclaration = p.declaration
  override val intDomains: UnionFindStorage[IntVarImplementation] = UnionFindStorage[IntVarImplementation, IntDomainStorage](p.intDomains, instantiateDomainStorage)
  override val intViews: ArrayBuffer[IntView] = new ArrayBuffer[IntView](p.intViews.length)
  for(i: IntView <- p.intViews)
    intViews += i.copy()

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