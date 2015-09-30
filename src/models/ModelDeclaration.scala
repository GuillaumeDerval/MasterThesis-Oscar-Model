package models

import models.uninstantiated.BaseModel
import vars.{IntVar, IntVarImplem}

import scala.util.DynamicVariable

/**
 * The declaration of a Model.
 */
class ModelDeclaration {
  implicit val modelDeclaration = this

  private val declared_model: Model = new BaseModel(this)
  private val current_model: DynamicVariable[Model] = new DynamicVariable[Model](declared_model)

  /**
   * Get the initial model
   */
  def getDeclaredModel = declared_model

  /**
   * Get the current model
   */
  def getCurrentModel = current_model.value

  /**
   * Apply the function func, which uses Var declared in this ModelDeclaration,
   * on the model (inheriting for this object too), temporarily changing the current model.
   * @param model: model on which to apply the function
   * @param func: function to apply
   */
  def applyFuncOnModel(model: Model)(func: => Unit) = {
    assert(model.declaration == this, "The model must be a sub-model of the declarated model " +
      "of this instance of ModelDeclaration")
    current_model.withValue(model)(func)
  }
}