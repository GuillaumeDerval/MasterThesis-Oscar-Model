package solvers

import models.Model

/**
 * Trait used by classes that provides a way to handle new solutions of a solver
 */
trait SolutionManager {
  private var on_solution: Model => Unit = null

  def onSolution = on_solution

  def onSolution(s: => Unit): Unit = onSolution((_) => s)

  def onSolution(o: Model => Unit): Unit = on_solution = o
}