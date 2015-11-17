package solvers.cp

import models.Model
import solvers.cp.decompositions.DecompositionStrategy

trait DistributedCPSolve[RetVal] {
  private var branching: Branching = null
  private var on_solution: Model => RetVal = null
  private var decomposition_strategy: DecompositionStrategy = null

  def getSearch = branching
  def setSearch(b: Branching): Unit = branching = b
  def setSearch(b: => Seq[oscar.algo.search.Alternative]): Unit = branching = Branching(b)

  def onSolution = on_solution
  def onSolution(s: => RetVal): Unit = onSolution((a) => s)
  def onSolution(o: Model => RetVal): Unit = on_solution = o

  def setDecompositionStrategy(d: DecompositionStrategy): Unit = decomposition_strategy = d
  def getDecompositionStrategy: DecompositionStrategy = decomposition_strategy
}