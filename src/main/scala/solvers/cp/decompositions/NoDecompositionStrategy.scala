package solvers.cp.decompositions

import constraints.Constraint
import models.UninstantiatedModel
import solvers.cp.SubproblemData
import vars.IntVar

class NoDecompositionStrategy extends DecompositionStrategy
{
  override def decompose(model: UninstantiatedModel, count: Int): List[(List[Constraint], SubproblemData)] = {
    List((List(), new SubproblemData(0, 0)))
  }
}