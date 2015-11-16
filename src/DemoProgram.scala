import constraints.AllDifferent
import models.ModelDeclaration
import solvers.cp._
import vars.IntVar
import algebra.IntExpression._
import algebra.BoolExpression._
import oscar.util._
import solvers.cp.Branching.{noAlternative, branch}

class DemoNQueens extends ModelDeclaration with DistributedCPSolve[String] /*CPSolve*/ {
  val nQueens = 17 // Number of queens
  val Queens = 0 until nQueens

  // Variables
  val queens = Array.fill(nQueens)(IntVar(0, nQueens - 1))

  // Constraints
  post(AllDifferent(queens))
  post(AllDifferent(Queens.map(i => queens(i) + i).toArray))
  post(AllDifferent(Queens.map(i => queens(i) - i).toArray))

  setSearch(Branching.binaryFirstFail(queens))
  onSolution {
    val s = queens.map(_.min).mkString("-")
    //println(s)
    s
  }
  setDecompositionStrategy(new ReginDecompositionStrategy(queens))
}

object DemoDistribute extends DistributedCPProgram(new DemoNQueens()) with App {
  val t0 = System.nanoTime()
  solve()
  val t1 = System.nanoTime()
  val elapsed = (t1.toDouble - t0.toDouble)/math.pow(10, 9)

  println("Elapsed time: " + elapsed + "s")
}

class DemoDistributedModel extends ModelDeclaration with CPSolve {
  val s = IntVar(0, 9)
  val e = IntVar(0, 9)
  val n = IntVar(0, 9)
  val d = IntVar(0, 9)
  val m = IntVar(0, 9)
  val o = IntVar(0, 9)
  val r = IntVar(0, 9)
  val y = IntVar(0, 9)

  // constraints
  post(s*1000 + e*100 + n*10 + d +
    m*1000 + o*100 + r*10 + e ==
    m*10000 + o*1000 + n*100 + e*10 + y)
  post(s > 0)
  post(m > 0)
  post(AllDifferent(Array(s,e,n,d,m,o,r,y)))

  setSearch(Branching.binaryFirstFail(Array(s,e,n,d,m,o,r,y)))

  setSearch {
    selectMin(Array(s,e,n,d,m,o,r,y))(!_.isBound)(_.size) match {
      case None => noAlternative
      case Some(x) =>
        val v = x.min
        branch { post(x == v) }{ post(x != v) }
    }
  }

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

object DemoProgram extends CPProgram with App {
  val x1 = IntVar(3, 4)
  val x2 = IntVar(20, 30)
  /*setSearch {
    println(x1.min) //min is not available in model var
    println(x2.max)
  }*/
  solve()
}