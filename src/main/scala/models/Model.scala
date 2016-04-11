package models

import constraints.Constraint
import misc.{ModelVarStorage}
import vars._

/**
 * Basic trait for all models
 */
trait Model extends Serializable {
  type IntVarImplementation <: IntVarImplem

  val declaration: ModelDeclaration
  var intRepresentatives: ModelVarStorage[IntVar, IntVarImplementation]
  var optimisationMethod: OptimisationMethod

  /**
   * Add a new variable with a new domain
   * @param domain: an IntVarImplementation
   * @return the id to the newly created variable
   */
  def addNewRepresentative(domain: IntVarImplementation): Int = {
    val r = intRepresentatives.add(domain)
    intRepresentatives = r._2
    r._1
  }

  /**
   * Get implementation of a Var
   * @param v
   * @return
   */
  def getRepresentative(v: IntVar): IntVarImplementation = intRepresentatives.get(v)

  /**
   * Apply a function on this model
   * @param func
   */
  def apply(func: => Unit): Unit = declaration.applyFuncOnModel(this)(func)

  /**
   * Post a new constraint
   * @param constraint
   */
  def post(constraint: Constraint): Unit

  /**
    * Called when the optimisation method have been updated
    */
  protected def optimisationMethodUpdated(): Unit

  /**
    * Minimize v
    * @param v
    */
  def minimize(v: IntVar): Unit = {
    optimisationMethod = new Minimisation(v)
    optimisationMethodUpdated()
  }

  /**
    * Maximize v
    * @param v
    */
  def maximize(v: IntVar): Unit = {
    optimisationMethod = new Minimisation(v)
    optimisationMethodUpdated()
  }
}
