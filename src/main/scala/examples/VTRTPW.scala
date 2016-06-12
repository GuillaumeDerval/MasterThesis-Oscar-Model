package examples

import algebra.Sum
import constraints._
import solvers.cp.branchings.Branching
import solvers.cp.decompositions.CartProdRefinement
import solvers.cp.{DistributedCPApp, DistributedCPAppConfig}
import vars.IntVar

import scala.collection.mutable.ArrayBuffer
import scala.io.Source


/**
  * Example of VRTPW, copied from the original one from OscaR-lib.
  * GNU GPL, OscaR Authors
  */
object VTRTPW extends DistributedCPApp[Int] {
  override lazy val config = new DistributedCPAppConfig {
    val file = trailArg[String](descr = "Path to the VRTPW instance")
  }
  val instanceFile = config.file()
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

  val pred = Array.fill(nSites)(IntVar(0, nSites))
  val succ = Array.fill(nSites)(IntVar(0, nSites))
  val vehicles = Array.fill(nSites)(IntVar(0, nVehicles))
  val starts = Array.tabulate(nSites)(i => IntVar(startsMin(i), startsMax(i)))
  val ends = Array.tabulate(nSites)(i => starts(i) + durations(i))
  val load = Array.fill(nVehicles)(IntVar(0, capacity))
  val totalDistance = IntVar(0, distances.flatten.sum)

  minimize(totalDistance)

  add(Inverse(pred, succ))
  add(Circuit(pred, false))
  add(MinAssignment(pred, distances, totalDistance)) // lower bound
  add(Sum(Sites)(s => distances(s)(pred(s))) == totalDistance)

  // Ensure the link between the TSPTW and the BinPacking
  for (c <- Customers) {
    add(StrongEq(vehicles(pred(c)), vehicles(c)))
    add(StrongEq(vehicles(succ(c)), vehicles(c)))
  }

  // Each vehicle starts and ends at its own depot
  for (v <- Vehicles) {
    val first = nCustomers + v
    val last = first + nVehicles
    add(vehicles(first) == v)
    add(vehicles(last) == v)
  }

  // Links the depots
  add(succ(Depots.max) == Depots.min)
  for (v <- 0 until nVehicles - 1) {
    val last = nCustomers + nVehicles + v
    val first = nCustomers + v + 1
    add(succ(last) == first)
  }

  // Capacity of each vehicle
  add(BinPacking(vehicles, demands, load))

  // Starting time of each delivery
  for (c <- Customers) {
    add(starts(c) >= ends(pred(c)) + distances(c)(pred(c)))
    add(starts(succ(c)) >= ends(c) + distances(c)(succ(c)))
  }

  for (v <- Vehicles) {
    val first = nCustomers + v
    val last = first + nVehicles
    add(starts(first) == startsMin(first))
    add(ends(last) == startsMax(last))
  }

  setSearch(Branching.binaryLastConflict(pred, i => -pred(i).maxRegret(distances(i)), i => pred(i).minBy(distances(i))))

  onSolution {totalDistance.min}

  setDecompositionStrategy(new CartProdRefinement(pred, Branching.naryStatic(pred)))

  val (stats, solutions) = solve()
  println(stats, solutions)
}

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