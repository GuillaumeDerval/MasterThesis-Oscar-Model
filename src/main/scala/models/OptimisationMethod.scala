package models

import oscar.cp.constraints.CPObjectiveUnit
import solvers.cp.branchings.Branching
import solvers.cp.branchings.Branching.Alternative
import vars.IntVar

trait OptimisationMethod

case class Maximisation(objective: IntVar) extends OptimisationMethod
case class Minimisation(objective: IntVar) extends OptimisationMethod
case class NoOptimisation() extends  OptimisationMethod

/**
  * Manages the bound, for optimization problems
  */
trait BoundaryManager {
  type B
  def get_boundary(): B
  def update_boundary(newval: B): Unit
}

/**
  * Particular BoundaryManager for integer bounds
  */
trait IntBoundaryManager extends BoundaryManager {
  type B = Int
}

/**
  * Synchronized boundary manager.
  * @param initial initial value
  */
class SynchronizedIntBoundaryManager(initial: Int) extends IntBoundaryManager {
  @volatile private var boundary = initial
  def get_boundary(): Int = boundary
  def update_boundary(newval: Int) = boundary = newval
}

/**
  * Wrapper for a search, that ensures the solver is up-to-date with the boundary
  * @param original original search
  * @param boundaryManager the manager
  * @param cpobjective the cp objective to enforce
  */
class IntBoundaryUpdateSearchWrapper(original: Branching,
                                     boundaryManager:IntBoundaryManager,
                                     cpobjective: CPObjectiveUnit) extends Branching {
  override def alternatives(): Seq[Alternative] = {
    original.alternatives().map((a: Alternative) => {
      () => {
        cpobjective.updateWorstBound(boundaryManager.get_boundary())
        cpobjective.best = boundaryManager.get_boundary()
        a()
      }
    })
  }
}

/**
  * Wrapper for a solution manager.
  */
object CPIntBoundaryUpdateSolutionWrapper {
  def apply(original: Model => Unit, boundaryManager:IntBoundaryManager, variable: IntVar): Model => Unit = {
    (model: Model) => {
      val v = model.getRepresentative(variable)
      if(v.isBound)
        boundaryManager.update_boundary(v.max)
      original(model)
    }
  }
}