package models.operators

import models.instantiated.InstantiatedCPModel
import models.uninstantiated.UninstantiatedModel

/**
 * Instantiate a model for a solve using CP
 */
object CPInstantiate extends ModelOperator {
  def apply(model: UninstantiatedModel): InstantiatedCPModel = {
    new InstantiatedCPModel(model)
  }
}