package models

import oscar.cp.CPIntVar
import oscar.cp.constraints.CPObjectiveUnit
import solvers.cp.Branching
import vars.IntVar

trait OptimisationMethod

case class Maximisation(val objective: IntVar) extends OptimisationMethod
case class Minimisation(val objective: IntVar) extends OptimisationMethod
case class NoOptimisation() extends  OptimisationMethod

trait BoundaryManager {
  type B
  def get_boundary(): B
  def update_boundary(newval: B): Unit
}

class SynchronizedIntBoundaryManager(initial: Int) extends BoundaryManager {
  type B = Int
  @volatile private var boundary = initial
  def get_boundary(): Int = boundary
  def update_boundary(newval: Int) = boundary = newval
}

class IntBoundaryUpdateSearchWrapper(original: oscar.algo.search.Branching,
                                     boundaryManager:SynchronizedIntBoundaryManager,
                                     cpobjective: CPObjectiveUnit) extends oscar.algo.search.Branching {
  override def alternatives(): Seq[Branching.Alternative] = {
    original.alternatives().map((a: Branching.Alternative) => {
      () => {
        cpobjective.updateWorstBound(boundaryManager.get_boundary())
        cpobjective.best = boundaryManager.get_boundary()
        a()
      }
    })
  }
}

object CPIntBoundaryUpdateSolutionWrapper {
  def apply(original: Model => Unit, boundaryManager:SynchronizedIntBoundaryManager, variable: IntVar): Model => Unit = {
    (model: Model) => {
      val v = model.getRepresentative(variable)
      if(v.isBound)
        boundaryManager.update_boundary(v.max)
      original(model)
    }
  }
}