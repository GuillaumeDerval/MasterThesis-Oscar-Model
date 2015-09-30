package models

import constraints.Constraint
import misc.UnionFindStorage
import vars.{Var, VarImplem}

import scala.collection.mutable

/**
 * Basic interface for all models
 */
trait Model {
  type Implementation <: VarImplem
  val declaration: ModelDeclaration
  val parent: Option[Model]
  val domains: UnionFindStorage[Implementation]

  val constraints = new mutable.MutableList[Constraint]

  /**
   * Add a new variable with a new domain
   * @param domain
   * @return
   */
  def add_new_var(domain: Implementation): Int = {
    domains.add(domain)
  }

  /**
   * Add a new variable, reusing a domain already existing
   * @param reuse
   * @return
   */
  def add_new_var_reuse(reuse: Var): Int = domains.add(reuse.varid)


  /**
   * Get implementation of a Var
   * @param v
   * @return
   */
  def get_implementation(v: Var): Implementation = domains.find(v.varid)

  /**
   * Apply a function on this model
   * @param func
   */
  def apply(func: => Unit): Unit = declaration.applyFuncOnModel(this)(func)
}
