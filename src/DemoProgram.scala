import constraints.AllDifferent
import models.ModelDeclaration
import solvers.cp.{Branchings, CPSearch}
import vars.IntVar
import algebra.IntExpression._
import algebra.BoolExpression._
import visualisation.ConstraintsVisualisation

class DemoDistributedModel extends ModelDeclaration with CPSearch {
  val s = IntVar(0, 9)
  val e = IntVar(0, 9)
  val n = IntVar(0, 9)
  val d = IntVar(0, 9)
  val m = IntVar(0, 9)
  val o = IntVar(0, 9)
  val r = IntVar(0, 9)
  val y = IntVar(0, 9)

  /*var c = s*1000 + e*100 + n*10 + d +
    m*1000 + o*100 + r*10 + e ==
    m*10000 + o*1000 + n*100 + e*10 + y
  new ConstraintsVisualisation(Array[Constraint](c), "").display()
  new ConstraintsVisualisation(Array[Constraint](SimplifySum(c)), "").display()*/

  // constraints
  post(s*1000 + e*100 + n*10 + d +
    m*1000 + o*100 + r*10 + e ==
    m*10000 + o*1000 + n*100 + e*10 + y)
  post(s > 0)
  post(m > 0)
  post(AllDifferent(Array(s,e,n,d,m,o,r,y)))

  setSearch(Branchings.binaryFirstFail(Array(s,e,n,d,m,o,r,y)))
  onSolution {
    println("-----")
    println("s=" + s.toString)
    println("e=" + e.toString)
    println("n=" + n.toString)
    println("d=" + d.toString)
    println("m=" + m.toString)
    println("o=" + o.toString)
    println("r=" + r.toString)
    println("y=" + y.toString)
    println("-----")
  }
}

object DemoDistribute extends CPProgram(new DemoDistributedModel()) with App {
  solve()
}

object DemoProgram extends CPProgram with App {
  val x1 = IntVar(3, 4)
  val x2 = IntVar(20, 30)
  /*setSearch {
    println(x1.min) //min is not available in model var
    println(x2.max)
  }*/
  solve()
}