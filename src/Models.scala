import scala.collection.mutable
import scala.util.DynamicVariable

/**
 * A model of a problem, which is independent from the type of solver you will use
 */
class Model
{
  implicit var current_solver: DynamicVariable[Solver] = null
  implicit val model = this

  //Note to myself: this should only contain variables *used*, not intermediate variables produced by scala
  val registered_variables = mutable.Set[Var]()

  /**
   * Register a new variable in this model
   * @param variable: the variable to register
   */
  def register_variable(variable: Var): Unit = {
    registered_variables += variable
  }
}

trait CPModelHelper extends Model {
  /**
   * Convert Var to CPVar, when appropriate.
   * TODO: should not be here, we should be independent from Solver!
   * @param variable: model variable
   * @tparam CPVarType: type of the return value, which must be a CPVar with type adequate for the given Var
   * @return the CPVar linked to this Var
   */
  implicit def var_to_cpvar[CPVarType <: CPVar](variable: Var): CPVarType = {
    current_solver.value.asInstanceOf[CPSolver].get_instance_var(variable).asInstanceOf[CPVarType]
  }
}

/**
 * Trait used by classes that provides a way to handle new solutions of a solver
 */
trait SolutionManager
{
  private var on_solution: Model => Unit = null
  def onSolution = on_solution
  def onSolution(o: Model => Unit): Unit = on_solution = o
  def onSolution(s: => Unit): Unit = onSolution((_) => s)
}

/**
 * Trait used by classes that provides a search for a CPSolver
 */
trait CPSearch extends SolutionManager
{
  private var search: Model => Unit = null
  def getSearch = search
  def setSearch(s: Model => Unit): Unit = search = s
  def setSearch(s: => Unit): Unit = setSearch((_) => s)
}