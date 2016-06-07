package examples

import constraints.{AllDifferent, Table}
import solvers.cp.DistributedCPApp
import solvers.cp.branchings.Branching
import vars.IntVar
import algebra.IntExpression._
import algebra.Sum
import solvers.cp.decompositions.CartProdRefinement

import scala.io.Source
import scala.spores._

/**
  * Created by dervalguillaume on 6/06/16.
  */
object LongestPath extends DistributedCPApp[Int] with App {

  // --- reading the data ---

  val lines = Source.fromFile("planar-n200.ins1.txt_com10_ins1").getLines.reduceLeft(_ + " " + _)

  val vals = lines.split("[ ,\t]").toList.filterNot(_ == "")
  var index = 0
  def next() = {
    index += 1
    vals(index - 1)
  }
  val nbNodes = next().toInt // last one is a dummy start/end to use circuit
  val nbArcs = next().toInt
  val nodes = 0 until nbNodes
  println("nbNodes:" + nbNodes + " nbArcs:" + nbArcs)
  val absent = -10000000
  val distMatrix = Array.fill(nbNodes, nbNodes)(absent)
  for (i <- 1 to nbArcs) {
    val from = next().toInt - 1
    val to = next().toInt - 1
    val w: Int = (next().toDouble * 100).toInt
    distMatrix(from)(to) = w
    distMatrix(to)(from) = w
  }

  def successors(i: Int): Set[Int] = nodes.filter(distMatrix(i)(_) != absent).toSet

  val outNodes = Array.tabulate(nbNodes)(i => nodes.filter(distMatrix(i)(_) != absent).toSet)
  val inNodes = Array.tabulate(nbNodes)(i => nodes.filter(distMatrix(i)(_) != absent).toSet)

  // set of valid transitions pair
  val tuples = (for (i <- nodes; j <- successors(i)) yield (i, j)).toSet

  val distMatrix_ : Array[Array[Int]] = Array.tabulate(nbNodes, nbNodes)((i, j) => if (distMatrix(i)(j) == absent) 0 else distMatrix(i)(j))

  val len = 12 // path lenth

  println("----------- trying with length:" + len + "-------------")

  val path: Array[IntVar] = Array.fill(len)(IntVar(0, nbNodes))

  val weight = Sum(0 until len - 1)(i => distMatrix_(path(i))(path(i + 1))).reify()

  for (i <- 0 until len - 1) {
    add(Table(path(i), path(i + 1), tuples)) // for each consecutive visits, give the possible valid transitions
  }
  post(AllDifferent(path))

  maximize(weight)
  setSearch(Branching.binaryFirstFail(path))
  setDecompositionStrategy(new CartProdRefinement(path, Branching.binaryFirstFail(path)))

  onSolution(spore {
    val x_ = weight
    () => {
      x_.max
    }
  })

  val stats = solve()
  println(stats)
}