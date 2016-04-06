package solvers.cp.decompositions

import models.instantiated.InstantiatedCPModel
import models.uninstantiated.UninstantiatedModel
import solvers.cp.SubproblemData
import vars.IntVar

/**
  * Sort subproblems by discrepancy
  */
object DiscrepancyDecompositionStrategy {
  def apply(sub: DecompositionStrategy): DecompositionStrategy = new DiscrepancyDecompositionStrategyMap(sub)
  def apply(sub: ClosureDecompositionStrategy): ClosureDecompositionStrategy = new DiscrepancyDecompositionStrategyClosure(sub)
}

/**
  * Sort subproblems by discrepancy (DecompositionStrategy version)
  *
  * @param sub the initial decomposition
  */
class DiscrepancyDecompositionStrategyMap(sub: DecompositionStrategy) extends DecompositionStrategy {
  def decompose(model: UninstantiatedModel, count: Int): List[(Map[IntVar, Int],SubproblemData)] = {
    sub.decompose(model, count).sortBy(_._2.discrepancy)
  }
}

/**
  * Sort subproblems by discrepancy (ClosureDecompositionStrategy version)
  *
  * @param sub the initial decomposition
  */
class DiscrepancyDecompositionStrategyClosure(sub: ClosureDecompositionStrategy) extends ClosureDecompositionStrategy {
  def decompose(model: UninstantiatedModel, count: Int): List[((InstantiatedCPModel) => Unit,SubproblemData)] = {
    sub.decompose(model, count).sortBy(_._2.discrepancy)
  }
}