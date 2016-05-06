package constraints

import algebra.IntExpression

/**
 * Imposes that a given BoolExpression is true
 * @param array
 */
case class StrongEq(a: IntExpression, b: IntExpression) extends Constraint {}
