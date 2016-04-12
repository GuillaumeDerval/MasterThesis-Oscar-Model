package models

import constraints.Constraint
import misc.ModelVarStorage
import vars.IntVar
import vars.domainstorage.DomainStorage
import vars.domainstorage.int._

abstract class InstantiatedModel(p: UninstantiatedModel) extends Model {
  override val declaration: ModelDeclaration = p.declaration
  override val intRepresentatives: ModelVarStorage[IntVar, IntVarImplementation] = ModelVarStorage[IntVar, IntVarImplementation, IntDomainStorage](p.intRepresentatives, instantiateDomainStorage)
  override val optimisationMethod: OptimisationMethod = p.optimisationMethod

  for(c <- p.constraints)
    post(c)

  protected def instantiateDomainStorage(v: DomainStorage): IntVarImplementation = {
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

  /**
    * Post a new constraint
    * @param constraint constraint to add
    */
  def post(constraint: Constraint): Unit

  /**
    * Post a new constraint
    * @param constraint constraint to add
    */
  def add(constraint: Constraint): Unit = post(constraint)

  /**
    * Post a new constraint
    * @param constraint constraint to add
    */
  def += (constraint: Constraint): Unit = post(constraint)
}