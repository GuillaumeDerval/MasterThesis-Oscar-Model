package examples

import algebra.Sum
import constraints.AllDifferent
import models.CPModel
import solvers.cp.DistributedCPApp
import vars.IntVar
import oscar.util.selectMin
import solvers.cp.branchings.Branching
import solvers.cp.decompositions.CartProdRefinement

import scala.io.Source
import scala.spores._

object QuadraticAssignment extends DistributedCPApp[Int] with App {

  // Read the data
  var lines = Source.fromFile("qap.txt").getLines.toList.filter(_ != "")
  val n = lines.head.toInt
  val N = 0 until n
  lines = lines.drop(1)
  var w: Array[Array[Int]] = Array() //weight matrix
  var d: Array[Array[Int]] = Array() //distance matrix
  for (i <- N) {
    w = w :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
    lines = lines.drop(1)
  }
  for (i <- N) {
    d = d :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
    lines = lines.drop(1)
  }

  //onSolution { println("solution " + x.mkString(",")) }

  // for each facilities, the location chosen for it
  val x = N map (v => IntVar(0, n-1))

  add(AllDifferent(x.toArray))

  val obj = Sum(N, N)((i, j) => d(x(i))(x(j)) * w(i)(j)).reify()

  onSolution(spore {
    val x_ = obj
    () => {
      x_.max
    }
  })

  minimize(obj)

  val search = Branching.fromAlternatives(spore{
        val _x = x
        (cp: CPModel) => {
          selectMin(_x)(y => !y.isBound)(y => y.size) match {
            case None => Branching.noAlternative
            case Some(y) => {
              val v = y.min
              Branching.branch(cp.post(y == v))(cp.post(y != v))
            }
          }
        }
    }
  )

  setSearch(search)
  setDecompositionStrategy(new CartProdRefinement(x,search))

  val stats = solve()
  println(stats)
}