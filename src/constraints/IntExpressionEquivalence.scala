package constraints

import algebra.IntExpression
import vars.IntVar

class IntExpressionEquivalence(val expr: IntExpression, val variable: IntVar) extends Constraint {}
