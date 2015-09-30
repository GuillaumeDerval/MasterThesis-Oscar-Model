import models.{CPSearch, ModelDeclaration}
import vars.IntVar

class DemoDistributedModel extends ModelDeclaration with CPSearch {
  val x1 = IntVar(2, 3)
  val x2 = IntVar(20, 30)

  x2.updateMax(25)

  setSearch {
    println(x2.max)
  }

  onSolution {
    println(x2.max)
  }
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