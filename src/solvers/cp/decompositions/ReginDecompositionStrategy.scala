package solvers.cp.decompositions

import constraints.Table
import misc.CartesianProduct
import models.NoOptimisation
import models.operators.CPInstantiate
import models.uninstantiated.{ChildModel, UninstantiatedModel}
import solvers.cp.{Branching, SubproblemData}
import vars.IntVar

import scala.collection.mutable

class ReginDecompositionStrategy(vars: Array[IntVar], search: (Array[IntVar]) => Branching = Branching.naryStatic(_)) extends DecompositionStrategy
{
  override def decompose(model: UninstantiatedModel, count: Int): List[(Map[IntVar, Int], SubproblemData)] = {
    if(count == 0) //no decomposition
      return List((Map[IntVar,Int](), new SubproblemData(CartesianProduct.computeLog(vars), model.optimisationMethod)))

    var nbSolutions = 1
    var retval: List[(Map[IntVar, Int], SubproblemData)] = null
    for(i <- vars.indices) {
      nbSolutions *= vars(i).size
      if(nbSolutions >= count || i == vars.length - 1) {
        retval = tryDecomposition(model, vars.take(i+1), retval)
        if(retval.size >= count || i == vars.length - 1)
          return retval
        nbSolutions = retval.size
      }
    }
    assert(false, "Decompose should always return a value")
    null
  }

  def tryDecomposition(vmodel: UninstantiatedModel, svars: Array[IntVar], oldvalues: List[(Map[IntVar, Int],SubproblemData)]): List[(Map[IntVar, Int],SubproblemData)] = {
    val vmodel2 = new ChildModel(vmodel)
    //Disable optimisation to avoid unbounded variables
    //val old_method = vmodel.optimisationMethod
    vmodel2.optimisationMethod = new NoOptimisation
    val cpmodel = CPInstantiate(vmodel2)
    //vmodel.optimisationMethod = old_method

    val list = new mutable.MutableList[(Map[IntVar, Int],SubproblemData)]

    cpmodel.declaration.applyFuncOnModel(cpmodel) {
      var currentDiscrepancy = -1
      cpmodel.cpSolver.onSolution {
        val m = new mutable.HashMap[IntVar, Int]
        for(v <- svars) {
          if(!v.isBound)
            throw new Exception()
          m += v -> v.min
        }

        list += ((m.toMap,new SubproblemData(CartesianProduct.computeLog(vars), vmodel.optimisationMethod, currentDiscrepancy)))
      }
      cpmodel.cpSolver.search({
        val b = search(svars).apply(cpmodel).alternatives()
        val trueDiscrepancy = currentDiscrepancy
        b.zipWithIndex.map((tuple) => {
          () => {
            currentDiscrepancy = trueDiscrepancy + tuple._2
            tuple._1()
          }
        })
      })
      cpmodel.cpSolver.startSubjectTo() {
        if(oldvalues != null && oldvalues.nonEmpty) {
          val vars = oldvalues.head._1.keys.toArray
          val values = oldvalues.map(m => vars.map(v => m._1(v))).toArray
          cpmodel.post(Table(vars, values))
        }
      }
    }

    list.toList
  }
}