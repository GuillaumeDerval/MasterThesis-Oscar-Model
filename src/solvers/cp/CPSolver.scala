package solvers.cp

import models.Model
import solvers.Solver

/**
 * A CPSolver
 * @param model: the model to solve
 */
class CPSolver(val model: Model) extends Solver {
  /**
   * Solve the model. This can only be called when the model has been instantiated
   * @param search: the search
   * @param on_solution: closure that will be called when the search finds a solution
   */
  def solve(search: Model => Unit, on_solution: Model => Unit): Unit = {
    search(model)
  }
}