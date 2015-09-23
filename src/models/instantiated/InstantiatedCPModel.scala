package models.instantiated

import models.uninstantiated.UninstantiatedModel
import vars.cp.CPVar
import vars.cp.int._
import vars.domainstorage.int.{AdaptableIntDomainStorage, IntervalDomainStorage, SetDomainStorage, SingletonDomainStorage}

/**
 * An instantiated model, containing CPVars as implementations
 * @param p: the parent Model
 */
class InstantiatedCPModel(p: UninstantiatedModel) extends InstantiatedModel(p) {
  override type Implementation = CPVar

  override protected def instantiateAdaptableIntDomainStorage(adaptable: AdaptableIntDomainStorage): CPVar = {
    new CPAdaptableIntVar(instantiateDomainStorage(adaptable.content).asInstanceOf[CPIntVar])
  }

  override protected def instantiateSetDomainStorage(set: SetDomainStorage): CPVar = {
    new CPSetIntVar(set.content.clone())
  }

  override protected def instantiateSingletonDomainStorage(singleton: SingletonDomainStorage): CPVar = {
    new CPSingletonIntVar(singleton.min)
  }

  override protected def instantiateIntervalDomainStorage(interval: IntervalDomainStorage): CPVar = {
    new CPIntervalIntVar(interval.min, interval.max)
  }
}