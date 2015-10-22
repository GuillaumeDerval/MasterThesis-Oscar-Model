import constraints.Constraint
import models.operators.SimplifySum
import models.{CPSearch, ModelDeclaration}
import vars.IntVar
import algebra.IntExpression._
import algebra.BoolExpression._
import visualisation.ConstraintsVisualisation

class DemoDistributedModel extends ModelDeclaration with CPSearch {
  val s = IntVar(0, 9, Some("S"))
  val e = IntVar(0, 9, Some("E"))
  val n = IntVar(0, 9, Some("N"))
  val d = IntVar(0, 9, Some("D"))
  val m = IntVar(0, 9, Some("M"))
  val o = IntVar(0, 9, Some("O"))
  val r = IntVar(0, 9, Some("R"))
  val y = IntVar(0, 9, Some("Y"))

  // constraints
  val c = s*1000 + e*100 + n*10 + d +
    m*1000 + o*100 + r*10 + e ==
    m*10000 + o*1000 + n*100 + e*10 + y

  new ConstraintsVisualisation(Array[Constraint](c)).display()
  new ConstraintsVisualisation(Array[Constraint](SimplifySum(c))).display()

  setSearch {}
  onSolution {}
}

object DemoDistribute extends CPProgram(new DemoDistributedModel()) with App {
  solve()
}

object DemoProgram extends CPProgram with App {
  val x1 = IntVar(3, 4)
  val x2 = IntVar(20, 30)
  setSearch {
    println(x1.min) //min is not available in model var
    println(x2.max)
  }
  solve()
}