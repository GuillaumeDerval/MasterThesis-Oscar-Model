package constraints

import algebra.IntExpression

/**
 * An equality constraint
 * @param x: first expression
 * @param y: second expression
 */
class Eq(val x: IntExpression, val y: IntExpression) extends Constraint {}