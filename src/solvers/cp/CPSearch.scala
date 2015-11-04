package solvers.cp

import solvers.SolutionManager

/**
 * Trait used by classes that provides a search for a solvers.cp.CPSolver
 */
trait CPSearch extends SolutionManager {
  private var branching: Branching = null
  def getSearch = branching
  def setSearch(b: Branching): Unit = {
    branching = b
  }
}