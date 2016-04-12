import algebra.Sum
import constraints._
import models.operators.SimplifySum
import solvers.cp
import solvers.cp.branchings.Branching
import solvers.cp.decompositions._
import solvers.cp.{DistributedCPApp, DistributedCPAppConfig}
import vars.IntVar

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.spores._

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

  post(m(n-1) < 120)

  onSolution(spore {
    val m_ = m
    () => {
      val v = m_.map(_.max).mkString(",")
      v
    }
  })

  apply(SimplifySum)

  setDecompositionStrategy(new CartesianProductRefinementDecompositionStrategy(m))
  val (stats, solutions) = solve()
  println(stats)
  println(solutions.last)
}

/** @author Renaud Hartert ren.hartert@gmail.com */
object VRPTW  extends cp.LocalParallelCPProgram[String] with App {
  this.subproblemsCount = 1000

  val instanceFile = "C102.txt"
  val instance = VRPTWParser.parse(instanceFile)

  // Data
  val nCustomers = instance.nCustomers
  val nVehicles = instance.nVehicles
  val nSites = instance.nSites
  val capacity = instance.capacity
  val Vehicles = instance.Vehicles
  val Customers = instance.Customers
  val Sites = instance.Sites
  val Depots = instance.Depots
  val outDepots = instance.Depots
  val inDepots = instance.Depots
  val demands = instance.demands
  val startsMin = instance.startsMin
  val startsMax = instance.startsMax
  val durations = instance.durations
  val coordinates = instance.coordinates
  val distances = instance.distances

  val pred = Array.fill(nSites)(IntVar(Sites.toSet))
  val succ = Array.fill(nSites)(IntVar(Sites.toSet))
  val vehicles = Array.fill(nSites)(IntVar(Vehicles.toSet))
  val starts = Array.tabulate(nSites)(i => IntVar(startsMin(i), startsMax(i)))
  val ends = Array.tabulate(nSites)(i => starts(i) + durations(i))
  val load = Array.fill(nVehicles)(IntVar(0, capacity))
  val totalDistance = IntVar(0, distances.flatten.sum)

  this.modelDeclaration.minimize(totalDistance)

  post(Inverse(pred, succ))
  post(Circuit(pred, false))
  post(MinAssignment(pred, distances, totalDistance)) // lower bound

  post(Sum(Sites)(s => (distances(s)(pred(s)))) == totalDistance)


  // Ensure the link between the TSPTW and the BinPacking
  for (c <- Customers) {
    post(vehicles(pred(c)) == vehicles(c))
    post(vehicles(succ(c)) == vehicles(c))
  }

  // Each vehicle starts and ends at its own depot
  for (v <- Vehicles) {
    val first = nCustomers + v
    val last = first + nVehicles
    post(vehicles(first) == v)
    post(vehicles(last) == v)
  }

  // Links the depots
  post(succ(Depots.max) == Depots.min)
  for (v <- 0 until nVehicles - 1) {
    val last = nCustomers + nVehicles + v
    val first = nCustomers + v + 1
    post(succ(last) == first)
  }

  // Capacity of each vehicle
  post(BinPacking(vehicles, demands, load))

  // Starting time of each delivery
  for (c <- Customers) {
    post(starts(c) >= ends(pred(c)) + distances(c)(pred(c)))
    post(starts(succ(c)) >= ends(c) + distances(c)(succ(c)))
  }

  for (v <- Vehicles) {
    val first = nCustomers + v
    val last = first + nVehicles
    post(starts(first) == startsMin(first))
    post(ends(last) == startsMax(last))
  }

  // Conflict set / max regret search
  setSearch(Branching.binaryLastConflict(pred, i => -pred(i).maxRegret(distances(i)), i => pred(i).minBy(distances(i))))
  //setSearch(Branching.binaryFirstFail(pred))

  //val visu = new VisualVRPTW(coordinates, succ, vehicles)

  setDecompositionStrategy(new ReginDecompositionStrategy(pred))

  onSolution {
    println(totalDistance)
    totalDistance.toString()
    //visu.updateTour()
  }

  val stats = solve()
  println(stats)
}

/*class VisualVRPTW(coordinates: Array[(Int, Int)], succ: Array[CPIntVar], vehicles: Array[CPIntVar]) {

  import oscar.visual._

  val Customers = 0 until coordinates.size
  val colors = VisualUtil.getRandomColors(vehicles.length, true)
  val frame = VisualFrame("Visualization")

  val tour = VisualTour(coordinates)
  frame.createFrame("VRPTW").add(tour)
  frame.pack()

  Customers.foreach(c => tour.nodeRadius(c, 1))

  // Updates the visualization
  def updateTour(): Unit = {
    Customers.foreach(i => {
      tour.edgeDest(i, succ(i).value)
      tour.edgeColor(i, colors(vehicles(i).value))
    })
    tour.repaint()
  }
}*/

class VRPTWInstance(
                     val nCustomers: Int,
                     val nVehicles: Int,
                     val nSites: Int,
                     val capacity: Int,
                     val demands: Array[Int],
                     val startsMin: Array[Int],
                     val startsMax: Array[Int],
                     val durations: Array[Int],
                     val coordinates: Array[(Int, Int)],
                     val distances: Array[Array[Int]]){
  def Customers = 0 until nCustomers
  def Vehicles = 0 until nVehicles
  def Sites = 0 until nSites
  def Depots = nCustomers until nCustomers + 2 * nVehicles
}

object VRPTWParser {
  def parse(filepath: String, scale: Int = 100): VRPTWInstance = {

    val lines = Source.fromFile(filepath).getLines

    // Drop 4 lines
    lines.next
    lines.next
    lines.next
    lines.next

    // Parse nVehices and capacity
    val line = lines.next.trim.split("[ ,\t]+")
    val nVehicles = line(0).toInt
    val capacity = line(1).toInt

    // Drop 5 lines
    lines.next
    lines.next
    lines.next
    lines.next

    val coordinatesBf = ArrayBuffer[(Int, Int)]()
    val demandsBf = ArrayBuffer[Int]()
    val startsMinBf = ArrayBuffer[Int]()
    val startsMaxBf = ArrayBuffer[Int]()
    val durationsBf = ArrayBuffer[Int]()

    while (lines.hasNext) {
      val line = lines.next
      if (!line.isEmpty) {
        val data = line.trim.split("[ ,\t]+")
        coordinatesBf.append((data(1).toInt, data(2).toInt))
        demandsBf.append(data(3).toInt)
        startsMinBf.append(data(4).toInt * scale)
        startsMaxBf.append(data(5).toInt * scale)
        durationsBf.append(data(6).toInt * scale)
      }
    }

    val nCustomers = durationsBf.length - 1
    val nSites = nCustomers + 2 * nVehicles
    val Sites = 0 until nSites

    val demands = new Array[Int](nSites)
    val startsMin = new Array[Int](nSites)
    val startsMax = new Array[Int](nSites)
    val durations = new Array[Int](nSites)
    val coordinates = new Array[(Int, Int)](nSites)

    for (s <- Sites) {
      val i = if (s >= nCustomers) 0 else s + 1
      demands(s) = demandsBf(i)
      startsMin(s) = startsMinBf(i)
      startsMax(s) = startsMaxBf(i)
      durations(s) = durationsBf(i)
      coordinates(s) = coordinatesBf(i)
    }

    // Distance matrix between sites
    val distances = Array.tabulate(nSites, nSites)((i, j) => {
      val (xi, yi) = coordinates(i)
      val (xj, yj) = coordinates(j)
      val dx = xi - xj
      val dy = yi - yj
      val distance = math.sqrt(dx * dx + dy * dy)
      (distance * scale).round.toInt
    })

    new VRPTWInstance(
      nCustomers,
      nVehicles,
      nSites,
      capacity,
      demands,
      startsMin,
      startsMax,
      durations,
      coordinates,
      distances
    )
  }
}
