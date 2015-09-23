package models.operators

import models.instantiated.{InstantiatedCPModel, InstantiatedModel}
import models.uninstantiated.UninstantiatedModel

/**
 * Instantiate a model for a solve using CP
 */
object CPInstantiate extends ModelOperator {
  def apply(model: UninstantiatedModel): InstantiatedModel = new InstantiatedCPModel(model)
}