package constraints

import algebra.IntExpression

/**
  * Table constraint
  * @param X
  * @param table
  */
case class Table(X: Array[IntExpression], table: Array[Array[Int]]) extends Constraint {}
