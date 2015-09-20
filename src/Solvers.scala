import scala.collection.mutable

abstract class Solver {}

class CPSolver(val model: Model) extends Solver {
  val model_to_instance = new mutable.HashMap[Var, CPVar]()
  //val instance_to_model = new mutable.HashMap[CPVar, mutable.Set[Var]] with mutable.MultiMap[CPVar, Var]

  def register_instance_var(model_var: Var, instance_var: CPVar): Unit = {
    model_to_instance += model_var -> instance_var
    //instance_to_model.addBinding(instance_var, model_var)
  }

  def get_instance_var(model_var: Var): CPVar = {
    model_to_instance.apply(model_var)
  }

  /**
   * Solve the model. This can only be called when the model has been instanciated
   * @param search: the search
   * @param on_solution: closure that will be called when the search finds a solution
   */
  def solve(search: Model => Unit, on_solution: Model => Unit): Unit = {
    search(model)
  }
}