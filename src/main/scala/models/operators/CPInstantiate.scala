package models.operators

import models.{CPModel, UninstantiatedModel}

/**
 * Instantiate a model for a solve using CP
 */
object CPInstantiate extends ModelOperator {
  def apply(model: UninstantiatedModel): CPModel = {
    new CPModel(model)
  }
}