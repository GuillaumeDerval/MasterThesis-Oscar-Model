package solvers.cp.decompositions

import models.NoOptimisation
import models.instantiated.InstantiatedCPModel
import models.operators.CPInstantiate
import models.uninstantiated.{ChildModel, UninstantiatedModel}
import oscar.cp.CPIntVar
import solvers.cp.Branching
import oscar.cp.constraints.InSet

import scala.collection.mutable

/**
  * Created by dervalguillaume on 5/02/16.
  */
class SearchDecompositionStrategy(search: Branching) extends DecompositionStrategy {
  var currentDepth = -1
  var currentPath: Array[Int] = null

  def decompose(model: UninstantiatedModel, count: Int): List[((InstantiatedCPModel) => Unit,SubproblemData)] = {
    var v: List[((InstantiatedCPModel) => Unit,SubproblemData)] = null
    var d: Int = 0
    do {
      d += 1
      v = tryDecomposition(model, d)
    } while(v.length < count)
    v
  }

  def customSearch(a: InstantiatedCPModel, maxDepth: Int): Seq[oscar.cp.Alternative] = {
    val base : Seq[oscar.cp.Alternative] = search.forModel(a).alternatives()
    val trueDepth = currentDepth+1
    if(trueDepth == maxDepth)
      oscar.cp.noAlternative
    else
      base.zipWithIndex.map((tuple) => {
        () => {
          currentDepth = trueDepth
          currentPath(currentDepth) = tuple._2
          tuple._1()
        }
      })
  }

  def tryDecomposition(model: UninstantiatedModel, maxDepth: Int): List[((InstantiatedCPModel) => Unit,SubproblemData)] = {
    currentDepth = -1
    currentPath = Array.tabulate(maxDepth)(_ => -1)

    val vmodel = new ChildModel(model)
    //Disable optimisation to avoid unbounded variables
    vmodel.optimisationMethod = new NoOptimisation


    val cpmodel = CPInstantiate(vmodel)

    val path_list = new mutable.MutableList[(Array[Int],SubproblemData)]
    var currentActions = mutable.Stack[() => Unit]()

    implicit val declaration = cpmodel.declaration

    declaration.applyFuncOnModel(cpmodel) {
      cpmodel.cpSolver.search(customSearch(cpmodel, maxDepth))
      cpmodel.cpSolver.onSolution {
        path_list += ((currentPath.clone().slice(0, currentDepth+1), new SubproblemData(0, model.optimisationMethod))) //todo
      }
      cpmodel.cpSolver.start()
    }

    path_list.toList.map(path => {
      ((newModel: InstantiatedCPModel) => {
        val newSearch = search.forModel(newModel)
        var currentAlternatives = newSearch.alternatives()
        for(i <- path._1; if i >= 0) {
          print(currentAlternatives.length)
          currentAlternatives(i)()
          currentAlternatives = search.forModel(newModel).alternatives()
        }
      }, path._2)
    })
  }
}