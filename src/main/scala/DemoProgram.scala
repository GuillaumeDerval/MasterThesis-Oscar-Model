import algebra.BoolExpression._
import algebra.IntExpression._
import algebra.Sum
import constraints.AllDifferent
import models.ModelDeclaration
import oscar.util.selectMin
import solvers.cp._
import solvers.cp.branchings.Branching
import solvers.cp.decompositions.old.SearchDecompositionStrategy
import vars.IntVar

class DemoNQueens extends ModelDeclaration with LocalDecomposedCPSolve[String] {
  val nQueens = 16 // Number of queens
  val Queens = 0 until nQueens

  // Variables
  val queens = Array.fill(nQueens)(IntVar(0, nQueens - 1))

  val toMin = Sum(queens.zipWithIndex.map((a: (IntVar, Int)) => a._1*(a._2*a._2))).reify()

  // Constraints
  post(AllDifferent(queens))
  post(AllDifferent(Queens.map(i => queens(i) + i).toArray))
  post(AllDifferent(Queens.map(i => queens(i) - i).toArray))

  setSearch(Branching.binaryFirstFail(queens/* ++ Seq(toMin)*/))
  onSolution {
    val s = queens.map(_.min).mkString("-")
    println(s)
    s
  }

  minimize(toMin)

  //setDecompositionStrategy(new NoDecompositionStrategy)
  //setDecompositionStrategy(new ReginDecompositionStrategy(queens))
  setDecompositionStrategy(new SearchDecompositionStrategy(Branching.binaryFirstFail(queens.splitAt(4)._1)))
}

/*class ATSP extends ModelDeclaration with DistributedCPSolve[String] {
  var lines = Source.fromFile("ftv70.atsp").getLines.toArray

  lines = lines.take(lines.size-1) // drop EOF
  val n = lines(3).split(":")(1).trim().toInt
  val dist = lines.drop(7).reduceLeft(_ + " " + _).split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)

  val distMatrixSucc = dist.sliding(n,n).toArray.map(_.toArray)

  distMatrixSucc.foreach(i => println(i.mkString("\t")))
  val succ = Array.fill(n)(IntVar(0, n-1))
  val obj = IntVar(0, 1000000-1)
  post(MinCircuit(succ, distMatrixSucc, obj))

  minimize(obj)
  //post(obj < 2125)

  setSearch({

    // Select the not yet bound city with the smallest number of possible successors
    selectMin(0 until n)(!succ(_).isBound)(succ(_).size) match {
      case None => noAlternative
      case Some(x) => {
        // Select the closest successors of the city x
        val v = selectMin(0 until n)(succ(x).hasValue(_))(distMatrixSucc(x)(_)).get
        branch(post(succ(x) == v))(post(succ(x) != v))
      }
    }
  })

  setDecompositionStrategy(new ReginDecompositionStrategy(succ))
}*/

object DemoDistribute extends LocalParallelCPProgram(new DemoNQueens()) with App {
  this.subproblemsCount = 1000 //20
  //this.subproblemsCount = 1
  this.threadsToLaunch = 1
  val t0 = System.nanoTime()
  solve()
  val t1 = System.nanoTime()
  val elapsed = (t1.toDouble - t0.toDouble)/math.pow(10, 9)

  println("Elapsed time: " + elapsed + "s")
}

class DemoDistributedModel extends ModelDeclaration with CPSolve[Unit] {
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
      case None => Branching.noAlternative
      case Some(x) =>
        val v = x.min
        Branching.branch { post(x == v) }{ post(x != v) }
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