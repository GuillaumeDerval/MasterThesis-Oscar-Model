package models

import constraints.Constraint
import misc.DynamicModelVariable
import vars.IntVar
import vars.domainstorage.int.IntDomainStorage

/**
 * The declaration of a Model.
 */
class ModelDeclaration extends Serializable {
  implicit val modelDeclaration = this

  private val current_model: DynamicModelVariable = new DynamicModelVariable()
  current_model.value = BaseModel(this)

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
    *
    * @param constraint
   */
  def post(constraint: Constraint): Unit = current_model.value match {
    case m: InstantiatedModel => postinstantiated(m, constraint)
    case m: UninstantiatedModel => postUninstantiated(m, constraint)
  }

  private def postinstantiated(model: InstantiatedModel, constraint: Constraint): Unit = {
    model.post(constraint)
  }
  private def postUninstantiated(model: UninstantiatedModel, constraint: Constraint): Unit = {
    current_model.value = model.post(constraint)
  }

  def minimize(v: IntVar) = current_model.value match {
    case m: UninstantiatedModel => current_model.value = m.minimize(v)
    case _ => throw new Exception("Cannot modify the optimisation method of an instantiated model")
  }

  def maximize(v: IntVar) = current_model.value match {
    case m: UninstantiatedModel => current_model.value = m.maximize(v)
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
}