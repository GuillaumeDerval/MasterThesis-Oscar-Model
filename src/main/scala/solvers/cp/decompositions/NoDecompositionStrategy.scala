package solvers.cp.decompositions

import models.UninstantiatedModel
import solvers.cp.SubproblemData
import vars.IntVar

class NoDecompositionStrategy extends DecompositionStrategy
{
  override def decompose(model: UninstantiatedModel, count: Int): List[(Map[IntVar, Int], SubproblemData)] = {
    List((new collection.immutable.HashMap, new SubproblemData(0, 0)))
  }
}