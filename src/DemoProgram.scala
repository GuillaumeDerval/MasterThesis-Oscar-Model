class DemoDistributedModel extends Model with CPModelHelper with CPSearch {
  val x1 = IntVar(2, 3)
  val x2 = IntVar(20, 30)

  setSearch {
    println(x1 print) //print is not available in model var
  }

  onSolution {
    println(x2 print)
  }
}

object DemoDistribute extends CPProgram with App {
  override val model = new DemoDistributedModel()
  solve()
}

object DemoProgram extends CPProgram with App {
  val x1 = IntVar(3, 4)
  val x2 = IntVar(20, 30)
  setSearch {
    println(x1 print) //print is not available in model var
    println(x2 print)
  }
  solve()
}