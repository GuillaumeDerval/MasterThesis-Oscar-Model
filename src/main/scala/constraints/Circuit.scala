package constraints

import algebra.IntExpression

case class Circuit(succ: Array[IntExpression], symmetric: Boolean) extends Constraint {}