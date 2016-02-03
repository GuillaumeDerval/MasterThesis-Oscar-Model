package solvers.cp.decompositions

import constraints.Table
import models.operators.CPInstantiate
import models.uninstantiated.UninstantiatedModel
import solvers.cp.Branching
import vars.IntVar

import scala.collection.mutable

class NoDecompositionStrategy extends DecompositionStrategy
{
  override def decompose(model: UninstantiatedModel, count: Integer): List[Map[IntVar, Int]] = {
    List(new collection.immutable.HashMap)
  }
}