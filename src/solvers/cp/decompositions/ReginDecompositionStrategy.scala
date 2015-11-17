package solvers.cp.decompositions

import constraints.Table
import models.operators.CPInstantiate
import models.uninstantiated.UninstantiatedModel
import solvers.cp.Branching
import vars.IntVar

import scala.collection.mutable

class ReginDecompositionStrategy(vars: Array[IntVar], search: (Array[IntVar]) => Branching = Branching.binaryFirstFail(_)) extends DecompositionStrategy
{
  override def decompose(model: UninstantiatedModel, count: Integer): List[Map[IntVar, Int]] = {
    var nbSolutions = 1
    var retval: List[Map[IntVar, Int]] = null
    for(i <- 0 until vars.length) {
      nbSolutions *= vars(i).size
      if(nbSolutions >= count) {
        retval = tryDecomposition(model, vars.take(i), retval)
        if(retval.size >= count || i == vars.length - 1)
          return retval
        nbSolutions = retval.size
      }
    }
    assert(false, "Decompose should always return a value")
    null
  }

  def tryDecomposition(model: UninstantiatedModel, svars: Array[IntVar], oldvalues: List[Map[IntVar, Int]]): List[Map[IntVar, Int]] = {
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
      cpmodel.cpSolver.startSubjectTo() {
        if(oldvalues != null && oldvalues.nonEmpty) {
          val vars = oldvalues.head.keys.toArray
          val values = oldvalues.map(m => vars.map(v => m(v))).toArray
          cpmodel.post(Table(vars, values))
        }
      }
    }

    list.toList
  }
}