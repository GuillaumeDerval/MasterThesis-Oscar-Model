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
  override type IntVarImplementation = CPIntVar

  override protected def instantiateAdaptableIntDomainStorage(adaptable: AdaptableIntDomainStorage): CPIntVar = {
    new CPAdaptableIntVar(instantiateDomainStorage(adaptable.content))
  }

  override protected def instantiateSetDomainStorage(set: SetDomainStorage): CPIntVar = {
    new CPSetIntVar(set.content.clone(), set.getRepresentativeName)
  }

  override protected def instantiateSingletonDomainStorage(singleton: SingletonDomainStorage): CPIntVar = {
    new CPSingletonIntVar(singleton.min, singleton.getRepresentativeName)
  }

  override protected def instantiateIntervalDomainStorage(interval: IntervalDomainStorage): CPIntVar = {
    new CPIntervalIntVar(interval.min, interval.max, interval.getRepresentativeName)
  }
}