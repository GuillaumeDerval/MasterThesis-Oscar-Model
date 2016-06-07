package models

import java.util.UUID

import constraints.Constraint
import misc.DynamicModelVariable
import models.operators.ModelOperator
import vars.IntVar
import vars.domainstorage.int.IntDomainStorage

/**
 * The declaration of a Model.
 */
class ModelDeclaration extends Serializable {
  implicit val modelDeclaration = this

  private val current_model: DynamicModelVariable = new DynamicModelVariable()
  current_model.value = BaseModel(this)

  val uuid = UUID.randomUUID() //used by serialiser to ensure that Var are not resending models with them

  /**
   * Get the current model
   */
  def getCurrentModel = current_model.value

  /**
   * Apply the function func, which uses Var declared in this ModelDeclaration,
   * on the model (inheriting for this object too), temporarily changing the current model.
    *
    * @param model: model on which to apply the function
   * @param func: function to apply
   */
  def apply[RetVal](model: Model)(func: => RetVal): RetVal = {
    assert(model.declaration == this, "The model must be a sub-model of the declared model " +
      "of this instance of ModelDeclaration")
    current_model.withValue(model)(func)
  }

  /**
    * Post a new constraint
    * @param constraint the constraint to post
   */
  def post(constraint: Constraint): Unit = current_model.value match {
    case m: InstantiatedModel => postinstantiated(m, constraint)
    case m: UninstantiatedModel => postUninstantiated(m, constraint)
    case null => println("WTF")
  }

  /**
    * Add a new constraint to the model
    * @param constraint the constraint to add
    */
  def add(constraint: Constraint): Unit = post(constraint)

  /**
    * Post for Instantiated models
    * @param model the model on which to post the constraint
    * @param constraint the constraint to post
    */
  private def postinstantiated(model: InstantiatedModel, constraint: Constraint): Unit = {
    model.post(constraint)
  }

  /**
    * Post for Uninstantiated models
    * @param model the model on which to post the constraint
    * @param constraint the constraint to post
    */
  private def postUninstantiated(model: UninstantiatedModel, constraint: Constraint): Unit = {
    current_model.value = model.post(constraint)
  }

  /**
    * Minimize on variable v
    * @param v variable to minimize
    */
  def minimize(v: IntVar) = current_model.value match {
    case m: UninstantiatedModel => current_model.value = m.minimize(v)
    case _ => throw new Exception("Cannot modify the optimisation method of an instantiated model")
  }

  /**
    * Maximize on variable v
    * @param v variable to maximize
    */
  def maximize(v: IntVar) = current_model.value match {
    case m: UninstantiatedModel => current_model.value = m.maximize(v)
    case _ => throw new Exception("Cannot modify the optimisation method of an instantiated model")
  }

  /**
    * Remove the optimisation method
    */
  def removeOptimization() = current_model.value match {
    case m: UninstantiatedModel => current_model.value = m.removeOptimisation()
    case _ => throw new Exception("Cannot modify the optimisation method of an instantiated model")
  }

  def addNewRepresentative(domain: IntDomainStorage): Int = current_model.value match {
    case m: UninstantiatedModel => {
      val r = m.withNewVariable(domain)
      current_model.value = r._2
      r._1
    }
    case _ => throw new Exception("Cannot add a new variable in an instantiated model")
  }

  /**
    * Apply a model operator
    * @param operator operator to apply
    */
  def apply[OutputType <: Model](operator: ModelOperator[OutputType]): Unit = current_model.value match {
    case m: UninstantiatedModel => current_model.value = operator(m)
    case _ => throw new Exception("Cannot apply an operator on an instantiated model")
  }
}