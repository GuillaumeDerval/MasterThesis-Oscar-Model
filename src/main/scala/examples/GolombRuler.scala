package examples

import constraints.AllDifferent
import solvers.cp.branchings.Branching
import solvers.cp.decompositions.{CartProdRefinement, DecompositionAddCartProdInfo, DepthIterativeDeepening}
import solvers.cp.{DistributedCPApp, DistributedCPAppConfig}
import vars.IntVar

import scala.spores._

/**
  * Example of ATSP, copied from the original one from OscaR-lib.
  * GNU GPL, OscaR Authors
  */
object GolombRuler extends DistributedCPApp[String] with App {
  override lazy val config = new DistributedCPAppConfig {
    val size = trailArg[Int](descr = "Size of the golomb ruler")
  }

  def increasing(y: Array[IntVar]) = {
    for (i <- 1 until y.length) {
      post(y(i - 1) < y(i))
    }
  }

  var n = config.size()

  val m = Array.fill(n)(IntVar(0,(1 << (n - 1))-1))

  post(m(0) == 0)

  increasing(m)

  // Number of marks and differences
  val n_d = (n*n-n)/2

  // Array of differences
  val d = Array.ofDim[IntVar](n_d)

  var k = 0
  for(i <- 0 until n-1) {
    for(j <- i+1 until n) {
      d(k) = (m(j)-m(i)).reify()
      post(d(k) >= ((j-i)*(j-i+1)/2))
      k += 1
    }
  }

  post(AllDifferent(d))

  if (n > 2)
    post(d(0) < d(n_d-1))

  minimize(m(n - 1))

  setSearch {
    Branching.binaryStatic(m)
  }

  post(m(n-1) < n*n)

  onSolution(spore {
    val m_ = m
    () => {
      val v = m_.map(_.max).mkString(",")
      println(v)
      v
    }
  })

  //apply(SimplifySum)

  setDecompositionStrategy(new CartProdRefinement(m, Branching.binaryStatic(m)))
  //setDecompositionStrategy(new DecompositionAddCartProdInfo(new DepthIterativeDeepening(Branching.naryStatic(m)), m))
  val (stats, solutions) = solve()
  println(stats)
  //println(solutions.last)
}
