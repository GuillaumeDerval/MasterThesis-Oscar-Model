package constraints

import algebra.IntExpression

/**
 * Imposes that a given BoolExpression is true
 * @param array
 */
case class AllDifferent(val array: Array[IntExpression]) extends Constraint {}
