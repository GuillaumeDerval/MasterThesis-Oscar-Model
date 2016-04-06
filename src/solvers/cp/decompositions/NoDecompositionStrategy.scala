package solvers.cp.decompositions

import models.uninstantiated.UninstantiatedModel
import solvers.cp.SubproblemData
import vars.IntVar

import scala.collection.mutable

class NoDecompositionStrategy extends DecompositionStrategy
{
  override def decompose(model: UninstantiatedModel, count: Int): List[(Map[IntVar, Int], SubproblemData)] = {
    List((new collection.immutable.HashMap, new SubproblemData(0, 0)))
  }
}