package constraints

import algebra.BoolExpression

/**
 * Imposes that a given BoolExpression is false
 * @param expr
 */
class BoolExpressionFalse(val expr: BoolExpression) extends Constraint {}
