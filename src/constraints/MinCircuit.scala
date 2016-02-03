package constraints

import algebra.IntExpression

case class MinCircuit(succ: Array[IntExpression], distMatrixSucc: Array[Array[Int]], cost: IntExpression) extends Constraint {}
