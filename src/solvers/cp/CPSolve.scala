package solvers.cp

import models.Model
import solvers.cp.branchings.Branching
import solvers.cp.decompositions.{ClosureDecompositionStrategy, DecompositionStrategy}

/**
  * Contains the needed data for a simple CPSolver: branching and solution management
  * @tparam RetVal
  */
trait CPSolve[RetVal] {
  private var branching: Branching = null
  private var on_solution: Model => RetVal = null

  def getSearch = branching
  def setSearch(b: Branching): Unit = branching = b
  def setSearch(b: => Seq[oscar.algo.search.Alternative]): Unit = branching = Branching(b)

  def onSolution = on_solution
  def onSolution(s: => RetVal): Unit = onSolution((a) => s)
  def onSolution(o: Model => RetVal): Unit = on_solution = o
}



