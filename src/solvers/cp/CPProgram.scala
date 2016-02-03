package solvers.cp

import constraints.Constraint
import models._
import models.instantiated.InstantiatedCPModel
import models.operators.CPInstantiate
import models.uninstantiated.UninstantiatedModel

/**
 * A program that uses a CP solver, with a single search
 *
 * Note: if you override "val model" with a model that have the CPSearch trait, the
 * search defined inside will be used if none is currently defined. The same goes 
 * for onSolution with SolutionManager/CPSearch
 */
class CPProgram(md: ModelDeclaration with CPSolve = new ModelDeclaration() with CPSolve) {
  implicit val program = this
  implicit val modelDeclaration = md

  def getDeclaredModel = modelDeclaration.getDeclaredModel
  def getCurrentModel = modelDeclaration.getCurrentModel

  def getSearch = md.getSearch
  def setSearch(b: Branching): Unit = md.setSearch(b)
  def setSearch(b: => Seq[oscar.algo.search.Alternative]): Unit = md.setSearch(b)
  def onSolution = md.onSolution
  def onSolution(s: => Unit): Unit = md.onSolution(s)
  def onSolution(o: Model => Unit): Unit = md.onSolution(o)

  def solve(): Unit = solve(modelDeclaration.getCurrentModel)

  def solve(model: Model): Unit = {
    model match {
      case m: UninstantiatedModel => solve(m)
      case m: InstantiatedCPModel => solve(m)
      case _ => sys.error("Trying to solve an instanciated model, but not CP compatible, is impossible")
    }
  }

  def solve(model: UninstantiatedModel): Unit = {
    solve(CPInstantiate(model))
  }

  /**
   * Solve the model, by instantiating it and starting the resolution
   */
  def solve(model: InstantiatedCPModel): Unit = {
    //Start the solver
    modelDeclaration.applyFuncOnModel(model) {
      model.cpSolver.onSolution {onSolution(model)}
      model.cpSolver.search(getSearch(model))
      println(model.cpSolver.start())
    }
  }

  /**
    * Post a new constraint
    * @param constraint
    */
  def post(constraint: Constraint): Unit = modelDeclaration.post(constraint)
}