package solvers.cp.decompositions

import constraints.Table
import misc.SubsetProduct
import models.operators.CPInstantiate
import models.uninstantiated.UninstantiatedModel
import solvers.cp.Branching
import vars.IntVar

import scala.collection.mutable

class ReginDecompositionStrategy2(vars: Array[IntVar], search: (Array[IntVar]) => Branching = Branching.binaryFirstFail(_)) extends DecompositionStrategy
{
  val varsSize = vars.map(i => i.values().size)

  override def decompose(model: UninstantiatedModel, count: Integer): List[Map[IntVar, Int]] = {
    var nbSolutions = 1
    var retval: List[Map[IntVar, Int]] = null
    val currentlySelected = new mutable.HashSet[Int]
    val currentlyUnselected = new mutable.HashSet[Int]
    currentlyUnselected ++= vars.indices

    while(true) {
      val chosenIdx = SubsetProduct(count/nbSolutions, varsSize)
      currentlySelected ++= chosenIdx
      currentlyUnselected --= chosenIdx

      retval = tryDecomposition(model, currentlySelected.toArray.map(i => vars(i)), retval)
      if(retval.size >= count || currentlyUnselected.isEmpty)
        return retval
      nbSolutions = retval.size
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