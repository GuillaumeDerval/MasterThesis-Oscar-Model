package models.operators

import models.instantiated.{InstantiatedModel, InstantiatedCPModel}
import models.uninstantiated.UninstantiatedModel

/**
 * Instantiate a model for a solve using CP
 */
object CPInstantiate extends ModelInstantiationOperator {
  override def apply(model: UninstantiatedModel): InstantiatedModel = new InstantiatedCPModel(model)
}