package solvers.cp

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
class CPProgram[RetVal](md: ModelDeclaration with CPSolve[RetVal] = new ModelDeclaration() with CPSolve[RetVal])
  extends ModelProxy[CPSolve[RetVal], RetVal](md)
{
  implicit val program = this

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
      model.cpSolver.onSolution {onSolution()}
      model.cpSolver.search(getSearch(model))
      println(model.cpSolver.start())
    }
  }
}