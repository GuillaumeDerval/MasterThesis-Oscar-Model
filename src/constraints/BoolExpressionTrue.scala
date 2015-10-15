package constraints

import algebra.BoolExpression

/**
 * Imposes that a given BoolExpression is true
 * @param expr
 */
class BoolExpressionTrue(val expr: BoolExpression) extends Constraint {}
