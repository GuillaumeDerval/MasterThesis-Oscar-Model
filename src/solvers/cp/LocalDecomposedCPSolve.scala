package solvers.cp

import solvers.cp.decompositions.ClosureDecompositionStrategy

/**
  * Allow to decompose subproblems for solving using closures; only for local solving
  * @tparam RetVal
  */
trait LocalDecomposedCPSolve[RetVal] extends CPSolve[RetVal] {
  private var decomposition_strategy: ClosureDecompositionStrategy = null
  def setDecompositionStrategy(d: ClosureDecompositionStrategy): Unit = decomposition_strategy = d
  def getDecompositionStrategy: ClosureDecompositionStrategy = decomposition_strategy
}
