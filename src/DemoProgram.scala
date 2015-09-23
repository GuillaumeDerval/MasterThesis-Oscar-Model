import models.{CPSearch, ModelDeclaration}
import vars.IntVar

class DemoDistributedModel extends ModelDeclaration with CPSearch {
  val x1 = IntVar(2, 3)
  val x2 = IntVar(20, 30)

  x2.updateMax(25)

  setSearch {
    println(x2.max) //il y a ici une conversion implicite de IntVar vers IntVarImpl
    // qui est/sera un supertype de CPIntVar.
    // En fonction du thread, la conversion implicite pointe vers un IntDomainStorage different.
  }

  onSolution {
    println(x2.max)
  }
}

object DemoDistribute extends CPProgram with App {
  override val modelDeclaration = new DemoDistributedModel()
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