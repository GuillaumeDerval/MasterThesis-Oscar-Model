import models.{CPSearch, ModelDeclaration}
import vars.IntVar
import algebra.IntExpression._

class DemoDistributedModel extends ModelDeclaration with CPSearch {
  val x1 = IntVar(2, 3)
  val x2 = IntVar(20, 30)

  x2.updateMax(25)

  val xx = Array.tabulate(5)(x => IntVar(x, 2*x))
  val xy = Array.tabulate(5)(x => x)

  val t1 = x2 == 25 //min=0, max=1
  val t2 = t1 + 2 //min=2, max=3
  val t3 = xx(x1) //min=2, max=6
  val t4 = t2 ~** t3 //min=4, max=729
  val t5 = xy(x1) //min=2, max=3
  val t6 = t4 - t5 //min=1, max=727
  val c = ((x2 == 25) + 2) ~** xx(x1) - xy(x1) //should be the same a t6

  val varFromC = c.reify()

  setSearch {
    println("x1 "+x1.min+" "+x1.max)
    println("x2 "+x2.min+" "+x2.max)
    println("t1 "+t1.min+" "+t1.max)
    println("t2 "+t2.min+" "+t2.max)
    println("t3 "+t3.min+" "+t3.max)
    println("t4 "+t4.min+" "+t4.max)
    println("t5 "+t5.min+" "+t5.max)
    println("t6 "+t6.min+" "+t6.max)
    println("c "+c.min+" "+c.max)
    println("varFromC "+varFromC.min+" "+varFromC.max)
  }

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