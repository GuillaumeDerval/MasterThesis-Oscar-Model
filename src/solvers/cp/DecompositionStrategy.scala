package solvers.cp

import models.instantiated.InstantiatedCPModel
import models.operators.CPInstantiate
import models.uninstantiated.UninstantiatedModel
import vars.IntVar

import scala.collection.mutable

/**
  * Created by dervalguillaume on 6/11/15.
  */
trait DecompositionStrategy {
  def decompose(model: UninstantiatedModel, count: Integer): List[Map[IntVar, Int]]
}

class ReginDecompositionStrategy(vars: Array[IntVar], search: (Array[IntVar]) => Branching = Branching.binaryFirstFail(_)) extends DecompositionStrategy
{
  override def decompose(model: UninstantiatedModel, count: Integer): List[Map[IntVar, Int]] = {
    var nbSolutions = 1
    for(i <- 0 until vars.length) {
      nbSolutions *= vars(i).size
      if(nbSolutions >= count) {
        val retval = tryDecomposition(model, vars.take(i))
        if(retval.size >= count || i == vars.length - 1)
          return retval
        nbSolutions = retval.size
      }
    }
    assert(false, "Decompose should always return a value")
    null
  }

  def tryDecomposition(model: UninstantiatedModel, svars: Array[IntVar]): List[Map[IntVar, Int]] = {
    val cpmodel = CPInstantiate(model)
    val list = new mutable.MutableList[Map[IntVar, Int]]

    cpmodel.declaration.applyFuncOnModel(cpmodel) {
      cpmodel.cpSolver.onSolution {
        val m = new mutable.HashMap[IntVar, Int]
        for(v <- svars)
          m += v -> v.min
        list += m.toMap
      }
      cpmodel.cpSolver.search(search(svars).apply(cpmodel))
      println(cpmodel.cpSolver.start())
    }

    list.toList
  }
}