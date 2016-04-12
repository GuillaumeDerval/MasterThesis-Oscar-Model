package models

import oscar.cp.constraints.CPObjectiveUnit
import solvers.cp.branchings.Branching
import vars.IntVar

trait OptimisationMethod

case class Maximisation(objective: IntVar) extends OptimisationMethod
case class Minimisation(objective: IntVar) extends OptimisationMethod
case class NoOptimisation() extends  OptimisationMethod

trait BoundaryManager {
  type B
  def get_boundary(): B
  def update_boundary(newval: B): Unit
}

trait IntBoundaryManager extends BoundaryManager {
  type B = Int
}

class SynchronizedIntBoundaryManager(initial: Int) extends IntBoundaryManager {
  @volatile private var boundary = initial
  def get_boundary(): Int = boundary
  def update_boundary(newval: Int) = boundary = newval
}

class IntBoundaryUpdateSearchWrapper(original: oscar.algo.search.Branching,
                                     boundaryManager:IntBoundaryManager,
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
  def apply(original: Model => Unit, boundaryManager:IntBoundaryManager, variable: IntVar): Model => Unit = {
    (model: Model) => {
      val v = model.getRepresentative(variable)
      if(v.isBound)
        boundaryManager.update_boundary(v.max)
      original(model)
    }
  }
}