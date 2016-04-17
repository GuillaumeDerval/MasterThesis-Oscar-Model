package solvers.cp.decompositions

import constraints.Constraint
import misc.CartesianProduct
import models.operators.CPInstantiate
import models.{CPModel, UninstantiatedModel}
import oscar.cp.core.NoSolutionException
import solvers.cp.SubproblemData
import solvers.cp.branchings.Branching
import solvers.cp.branchings.Branching.{Alternative, BranchingInstantiator}
import vars.IntVar

import scala.collection.mutable

class CartesianProductDecompositionStrategy(allVars: Array[IntVar], search: BranchingInstantiator) extends ClosureDecompositionStrategy {

  def this(allVars: Array[IntVar], decompVars: Array[IntVar]) = this(allVars, Branching.naryStatic(decompVars))
  def this(allVars: Array[IntVar]) = this(allVars, allVars)

  var currentDepth = -1
  var currentDiscrepancy = -1
  var currentPath: Array[Int] = null

  def decompose(model: UninstantiatedModel, count: Int): List[((CPModel) => Unit,SubproblemData)] = {
    if(count == 0) //no decomposition
      return List[((CPModel) => Unit,SubproblemData)]()

    val initialLog = CartesianProduct.computeLog(allVars)

    var decomp: List[((CPModel) => Unit,SubproblemData)] = List[((CPModel) => Unit,SubproblemData)]()
    var currentThreshold = initialLog - Math.log(count) //divide real value by count
    while(decomp.size < count) {
      println("Try with "+currentThreshold.toString)
      decomp = tryDecomposition(model, currentThreshold)
      println("Found "+decomp.size.toString+" subproblems")
      currentThreshold = currentThreshold - Math.log(2.0d)
    }
    decomp
  }

  def customSearch(a: CPModel, threshold: Double): Seq[oscar.cp.Alternative] = {
    val base : Seq[Alternative] = search(a).alternatives()
    val trueDepth = currentDepth+1
    val trueDiscrepancy = currentDiscrepancy
    val currentCP = CartesianProduct.computeLog(allVars)
    if(currentCP <= threshold) {
      oscar.cp.noAlternative
    }
    else {
      if(trueDepth >= currentPath.length)
        currentPath = Array.tabulate(currentPath.length*2)(i => if(i < currentPath.length) currentPath(i) else -1)

      base.zipWithIndex.map((tuple) => {
        () => {
          currentDepth = trueDepth
          currentDiscrepancy = trueDiscrepancy + tuple._2
          currentPath(currentDepth) = tuple._2
          tuple._1()
        }
      })
    }
  }

  def tryDecomposition(model: UninstantiatedModel, threshold: Double): List[((CPModel) => Unit,SubproblemData)] = {
    currentDepth = -1
    currentPath = Array.tabulate(32)(_ => -1) //32 should be enough for decomposition
    currentDiscrepancy = 0

    val vmodel = model.removeOptimisation()


    val cpmodel = CPInstantiate(vmodel)

    val path_list = new mutable.MutableList[(Array[Int], SubproblemData)]
    var currentActions = mutable.Stack[() => Unit]()

    implicit val declaration = cpmodel.declaration

    declaration.apply(cpmodel) {
      cpmodel.cpSolver.search(customSearch(cpmodel, threshold))
      cpmodel.cpSolver.onSolution {
        path_list += ((currentPath.clone().slice(0, currentDepth+1), new SubproblemData(CartesianProduct.computeLog(allVars), model.optimisationMethod, currentDiscrepancy)))
      }
      cpmodel.cpSolver.start()
    }

    path_list.toList.map(path_with_data => {
      ((newModel: CPModel) => {
        val s = search(newModel)
        var currentAlternatives = s.alternatives()
        for(i <- path_with_data._1) {
          currentAlternatives(i)()
          currentAlternatives = s.alternatives()
        }
      }, path_with_data._2)
    })
  }
}

class CartesianProductRefinementDecompositionStrategy(allVars: Array[IntVar]) extends DecompositionStrategy {

  class SubproblemInfo(val assignment: List[Constraint], val cartesianProduct: Double, val path: List[Int]) extends Ordered[SubproblemInfo] {
    override def compare(that: SubproblemInfo): Int = cartesianProduct.compare(that.cartesianProduct)
  }

  def decompose(model: UninstantiatedModel, count: Int): List[(List[Constraint],SubproblemData)] = {
    if(count == 0) //no decomposition
      return List[(List[Constraint],SubproblemData)]()

    //Initialise a CP Model
    val vmodel = model.removeOptimisation()
    val cpmodel = CPInstantiate(vmodel)

    //Init the queue that will order the subproblems by cartesian product
    val q = mutable.PriorityQueue[SubproblemInfo]()
    q += new SubproblemInfo(List(), CartesianProduct.computeLog(allVars), List())

    cpmodel.declaration.apply(cpmodel) {
      while(q.size < count) {
        //Dequeue the largest subproblem, and compute its domain
        val sp = q.dequeue()
        cpmodel.cpSolver.pushState()
        for(c <- sp.assignment)
          cpmodel.post(c)

        //Find the first possible value without unary domain
        var variable: IntVar = null
        for(v <- allVars; if variable == null)
          if(v.size != 1)
            variable = v
        assert(null != variable)

        //For each of the possible value, create a new subproblem
        for(i <- variable.toList.sorted.zipWithIndex) {
          cpmodel.cpSolver.pushState()
          try
          {
            cpmodel.post(variable == i._1)
            //cpmodel.cpSolver.post(cpmodel.getRepresentative(variable).asInstanceOf[CPIntVar] == i)

            q += new SubproblemInfo((variable == i._1) :: sp.assignment, CartesianProduct.computeLog(allVars), sp.path ++ List(i._2))
          }
          catch { case nosol: NoSolutionException => }
          cpmodel.cpSolver.pop()
        }

        //Do not forget to pop the state
        cpmodel.cpSolver.pop()
      }
    }

    val r = q.toList.sortWith((a, b) => {
      var ok = false
      var result = false
      for((i,j) <- a.path.zip(b.path); if !ok) {
        if(i < j){
          ok = true
          result = true
        }
        else if(j < i) {
          ok = true
          result = false
        }
      }
      result
    })

    r.map(sp => {
      val spd = new SubproblemData(sp.cartesianProduct, model.optimisationMethod, sp.path.sum)
      (sp.assignment, spd)
    })
  }
}