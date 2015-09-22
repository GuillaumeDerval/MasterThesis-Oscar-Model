package models.operators

import models.instantiated.InstantiatedModel

/**
 * An operator that produces an instanciated child model
 */
trait ModelInstantiationOperator extends ModelOperator {
  type OutputModelType = InstantiatedModel
}
