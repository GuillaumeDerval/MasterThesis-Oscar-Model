package models.operators

import models.Model
import models.uninstantiated.UninstantiatedModel

/**
 * Base for any model operator. You should use more specialized version such as ModelInstantiationOperator and
 * ChildModelOperator
 */
trait ModelOperator {
  type OutputModelType <: Model
  def apply(model: UninstantiatedModel): OutputModelType
}



