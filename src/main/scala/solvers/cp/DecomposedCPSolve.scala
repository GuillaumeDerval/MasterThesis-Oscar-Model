package solvers.cp

import solvers.cp.decompositions.DecompositionStrategy

/**
  * Allow to decompose into subproblems for solving in a distributed environment
  * @tparam RetVal
  */
trait DecomposedCPSolve[RetVal] extends CPSolve[RetVal] {
  private var decomposition_strategy: DecompositionStrategy = null
  def setDecompositionStrategy(d: DecompositionStrategy): Unit = decomposition_strategy = d
  def getDecompositionStrategy: DecompositionStrategy = decomposition_strategy
}
