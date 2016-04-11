package solvers.cp

import solvers.cp.branchings.Branching

/**
  * Contains the needed data for a simple CPSolver: branching and solution management
  * @tparam RetVal
  */
trait CPSolve[RetVal] {
  protected var branching: Branching = null
  protected var on_solution: () => RetVal = null

  def getSearch = branching
  def setSearch(b: Branching): Unit = branching = b
  def setSearch(b: => Seq[oscar.algo.search.Alternative]): Unit = branching = Branching(b)

  def onSolution = on_solution
  def onSolution(o: => RetVal): Unit = on_solution = () => o
}



