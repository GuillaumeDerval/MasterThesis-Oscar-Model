package models.operators

import models.uninstantiated.ChildModel

/**
 * An operator that produces an uninstanciated child model
 */
trait ChildModelOperator extends ModelOperator {
  type OutputModelType = ChildModel
}
