package solvers.cp.decompositions

import models.NoOptimisation
import models.operators.CPInstantiate
import models.uninstantiated.{ChildModel, UninstantiatedModel}
import vars.IntVar

import scala.collection.mutable

/**
  * Created by dervalguillaume on 5/02/16.
  */
class SearchDecompositionStrategy extends DecompositionStrategy {
  def decompose(model: UninstantiatedModel, count: Int): List[Map[IntVar, Int]] = {
    var v: List[Map[IntVar, Int]] = null
    var d: Int = 0
    do {
      d += 1
      v = tryDecomposition(model, d)
    } while(v.length < count)
    v
  }

  def tryDecomposition(model: UninstantiatedModel, maxDepth: Int): List[Map[IntVar, Int]] = {
    val vmodel = new ChildModel(model)
    //Disable optimisation to avoid unbounded variables
    vmodel.optimisationMethod = new NoOptimisation
    val cpmodel = CPInstantiate(vmodel)

    val list = new mutable.MutableList[Map[IntVar, Int]]

    cpmodel.declaration.applyFuncOnModel(cpmodel) {
      cpmodel.cpSolver.onSolution {
        val m = new mutable.HashMap[IntVar, Int]
        for(v <- svars) {
          if(!v.isBound)
            throw new Exception()
          m += v -> v.min
        }

        list += m.toMap
      }

  }
}