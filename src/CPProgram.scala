import models._
import models.instantiated.InstantiatedCPModel
import models.operators.CPInstantiate
import models.uninstantiated.UninstantiatedModel
import solvers.CPSolver
import vars.{IntVar, IntVarImplem}

/**
 * A program that uses a CP solver, with a single search
 *
 * Note: if you override "val model" with a model that have the CPSearch trait, the
 * search defined inside will be used if none is currently defined. The same goes 
 * for onSolution with SolutionManager/CPSearch
 */
class CPProgram(md: ModelDeclaration = new ModelDeclaration()) extends CPSearch {
  implicit val program = this
  implicit val modelDeclaration = md

  def getDeclaredModel = modelDeclaration.getDeclaredModel
  def getCurrentModel = modelDeclaration.getCurrentModel

  def solve(): Unit = solve(modelDeclaration.getCurrentModel)

  def solve(model: Model): Unit = {
    if (model.isInstanceOf[UninstantiatedModel])
      solve(model.asInstanceOf[UninstantiatedModel])
    else {
      assert(model.isInstanceOf[InstantiatedCPModel], "Trying to solve an instanciated model, but not CP compatible, is impossible")
      solve(model.asInstanceOf[InstantiatedCPModel])
    }
  }

  def solve(model: UninstantiatedModel): Unit = {
    solve(CPInstantiate(model))
  }

  /**
   * Solve the model, by instantiating it and starting the resolution
   */
  def solve(model: InstantiatedCPModel): Unit = {
    //Create solvers.CPSolver
    val solver = new CPSolver(model)

    //Get the search
    var search = getSearch
    if (search == null && modelDeclaration.isInstanceOf[CPSearch])
      search = modelDeclaration.asInstanceOf[CPSearch].getSearch

    //Get onSolution
    var on_solution = onSolution
    if (on_solution == null && modelDeclaration.isInstanceOf[SolutionManager])
      on_solution = modelDeclaration.asInstanceOf[SolutionManager].onSolution

    //Start the solver
    modelDeclaration.applyFuncOnModel(model) {
      search(model)
    }
  }
}