package solvers.cp.decompositions

import constraints.Constraint
import models.{CPModel, UninstantiatedModel}
import solvers.cp.SubproblemData

/**
  * Sort subproblems by discrepancy
  */
object DiscrepancyDecompositionStrategy {
  def apply(sub: DecompositionStrategy): DecompositionStrategy = new DiscrepancyDecompositionStrategyConstraintList(sub)
  def apply(sub: ClosureDecompositionStrategy): ClosureDecompositionStrategy = new DiscrepancyDecompositionStrategyClosure(sub)
}

/**
  * Sort subproblems by discrepancy (DecompositionStrategy version)
  *
  * @param sub the initial decomposition
  */
class DiscrepancyDecompositionStrategyConstraintList(sub: DecompositionStrategy) extends DecompositionStrategy {
  def decompose(model: UninstantiatedModel, count: Int): List[(List[Constraint],SubproblemData)] = {
    sub.decompose(model, count).sortBy(_._2.discrepancy)
  }
}

/**
  * Sort subproblems by discrepancy (ClosureDecompositionStrategy version)
  *
  * @param sub the initial decomposition
  */
class DiscrepancyDecompositionStrategyClosure(sub: ClosureDecompositionStrategy) extends ClosureDecompositionStrategy {
  def decompose(model: UninstantiatedModel, count: Int): List[((CPModel) => Unit,SubproblemData)] = {
    sub.decompose(model, count).sortBy(_._2.discrepancy)
  }
}