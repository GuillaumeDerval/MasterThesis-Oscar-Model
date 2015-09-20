/**
 * A program that uses a CP solver, with a single search
 *
 * Note: if you override "val model" with a model that have the CPSearch trait, the
 * search defined inside will be used if none is currently defined. The same goes 
 * for onSolution with SolutionManager/CPSearch
 */
trait CPProgram extends CPSearch {
  implicit val program = this
  implicit val model: Model = new Model()

  /**
   * Solve the model, by instanciating it and starting the resolution
   */
  def solve(): Unit = {
    //Create CPSolver
    implicit val solver = new CPSolver(model)

    //Get the search
    var search = getSearch
    if(search == null && model.isInstanceOf[CPSearch])
      search = model.asInstanceOf[CPSearch].getSearch

    //Get onSolution
    var on_solution = onSolution
    if(on_solution == null && model.isInstanceOf[SolutionManager])
      search = model.asInstanceOf[SolutionManager].onSolution

    //Instanciate all variables
    model.registered_variables.foreach(_.instanciate(solver))

    //Start the solver
    model.current_solver = new NotReallyDynamicVariable[Solver](null)
    model.current_solver.withValue(solver) {
      solver.solve(search, onSolution)
    }
  }

  implicit def var_to_cpvar[CPVarType <: CPVar](variable: Var): CPVarType = {
    model.current_solver.value.asInstanceOf[CPSolver].get_instance_var(variable).asInstanceOf[CPVarType]
  }
}