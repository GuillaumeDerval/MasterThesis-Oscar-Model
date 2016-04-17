package solvers.cp.decompositions

import misc.CartesianProduct
import models.operators.CPInstantiate
import models.{CPModel, UninstantiatedModel}
import solvers.cp.SubproblemData
import solvers.cp.branchings.Branching
import solvers.cp.branchings.Branching.BranchingInstantiator
import vars.IntVar

import scala.collection.mutable

class ConfidenceDecompositionStrategy(allVars: Array[IntVar], search: BranchingInstantiator, confidence: Double) extends ClosureDecompositionStrategy {

  def this(allVars: Array[IntVar], decompVars: Array[IntVar], confidence: Double) = this(allVars, Branching.naryStatic(decompVars), confidence)
  def this(allVars: Array[IntVar], confidence: Double) = this(allVars, allVars, confidence)

  var currentDepth = -1
  var currentDiscrepancy = -1
  var currentAmount = -1
  var currentPath: Array[Int] = null

  def decompose(model: UninstantiatedModel, count: Int): List[((CPModel) => Unit,SubproblemData)] = {
    if(count == 0) //no decomposition
      return List[((CPModel) => Unit,SubproblemData)]()
    tryDecomposition(model, count)
  }

  def customSearch(search: Branching): Seq[oscar.cp.Alternative] = {
    val base : Seq[oscar.cp.Alternative] = search.alternatives()

    val trueDepth = currentDepth+1
    val trueDiscrepancy = currentDiscrepancy
    val trueAmount = currentAmount
    if(trueAmount == 1 || base.length == 0) {
      oscar.cp.noAlternative
    }
    else {
      if(trueDepth >= currentPath.length)
        currentPath = Array.tabulate(currentPath.length*2)(i => if(i < currentPath.length) currentPath(i) else -1)

      val amounts = Array.tabulate(base.length)(i => 1)
      amounts(0) = trueAmount
      for(i <- 0 until amounts.length-1) {
        val cca = amounts(i)
        amounts(i) = Math.max((cca.toDouble * confidence).toInt, 1)
        amounts(i+1) = Math.max(cca - amounts(i), 1)
      }

      base.zipWithIndex.map((tuple) => {
        () => {
          currentDepth = trueDepth
          currentDiscrepancy = trueDiscrepancy + tuple._2
          currentPath(currentDepth) = tuple._2
          currentAmount = amounts(tuple._2)
          tuple._1()
        }
      })
    }
  }

  def tryDecomposition(model: UninstantiatedModel, count: Int): List[((CPModel) => Unit,SubproblemData)] = {
    currentDepth = -1
    currentPath = Array.tabulate(32)(_ => -1) //32 should be enough for decomposition
    currentDiscrepancy = 0
    currentAmount = count

    val vmodel = model.removeOptimisation()

    val cpmodel = CPInstantiate(vmodel)

    val path_list = new mutable.MutableList[(Array[Int], SubproblemData)]
    var currentActions = mutable.Stack[() => Unit]()

    implicit val declaration = cpmodel.declaration

    declaration.apply(cpmodel) {
      val s = search(cpmodel)
      cpmodel.cpSolver.search(customSearch(s))
      cpmodel.cpSolver.onSolution {
        path_list += ((currentPath.clone().slice(0, currentDepth), new SubproblemData(CartesianProduct.computeLog(allVars), model.optimisationMethod, currentDiscrepancy)))
      }
      cpmodel.cpSolver.start()
    }

    path_list.toList.map(path_with_data => {
      ((newModel: CPModel) => {
        val newSearch = search(newModel)
        var currentAlternatives = newSearch.alternatives()
        for(i <- path_with_data._1) {
          currentAlternatives(i)()
          currentAlternatives = newSearch.alternatives()
        }
      }, path_with_data._2)
    })
  }
}