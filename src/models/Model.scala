package models

import constraints.Constraint
import misc.UnionFindStorage
import vars._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Basic interface for all models
 */
trait Model {
  type IntVarImplementation <: IntVarImplem
  val declaration: ModelDeclaration
  val parent: Option[Model]
  val intDomains: UnionFindStorage[IntVarImplementation]
  val intViews: ArrayBuffer[IntView]

  val constraints = new mutable.MutableList[Constraint]

  /**
   * Add a variable implementation to the UnionFindStorage
   * @param domain: an IntVarImplementation
   * @return the id to the newly created implementation
   */
  def addNewIntVarImplementation(domain: IntVarImplementation): Int = intDomains.add(domain)

  /**
   * Add a variable view to the list of views
   * @param view: an IntView
   * @return the id to the newly created view
   */
  def addNewIntView(view: IntView): Int = {intViews += view; intViews.length-1}

  /**
   * Get view of an IntVar
   * @param v an IntVar
   * @return
   */
  def getIntView(v: Int): IntView = intViews(v)

  /**
   * Get implementation of a Var
   * @param v the id to an implementation
   * @return
   */
  def getIntVarImplementation(v: Int): IntVarImplementation = intDomains.find(v)

  /**
   * Apply a function on this model
   * @param func
   */
  def apply(func: => Unit): Unit = declaration.applyFuncOnModel(this)(func)
}
