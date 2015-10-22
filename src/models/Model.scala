package models

import constraints.{ConstraintPower, Constraint}
import constraints.ConstraintPower.ConstraintPower
import misc.UnionFindStorage
import vars.{IntVar, IntVarImplem, Var, VarImplem}

import scala.collection.mutable

/**
 * Basic trait for all models
 */
trait Model {
  type IntVarImplementation <: IntVarImplem
  val declaration: ModelDeclaration
  val parent: Option[Model]
  val intRepresentatives: UnionFindStorage[IntVarImplementation]

  /**
   * Add a new variable with a new domain
   * @param domain: an IntVarImplementation
   * @return the id to the newly created variable
   */
  def addNewRepresentative(domain: IntVarImplementation): Int = intRepresentatives.add(domain)

  /**
   * Add a new variable, reusing a domain already existing
   * @param reuse
   * @return
   */
  def reuseRepresentative(reuse: IntVar): Int = intRepresentatives.add(reuse.varid)


  /**
   * Get implementation of a Var
   * @param v
   * @return
   */
  def getRepresentative(v: IntVar): IntVarImplementation = intRepresentatives.find(v.varid)

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
}
