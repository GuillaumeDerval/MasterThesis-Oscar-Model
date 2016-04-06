package solvers.cp.decompositions

import java.util.concurrent.LinkedBlockingQueue

import models._
import models.instantiated.InstantiatedCPModel
import models.uninstantiated.UninstantiatedModel
import vars.IntVar

/**
  * Created by dervalguillaume on 6/11/15.
  */
trait DecompositionStrategy {
  def decompose(model: UninstantiatedModel, count: Int): List[((InstantiatedCPModel) => Unit,SubproblemData)]
}

trait SimpleDecompositionStrategy extends DecompositionStrategy{
  def decomposeToMap(model: UninstantiatedModel, count: Int): List[(Map[IntVar, Int],SubproblemData)]

  def decompose(model: UninstantiatedModel, count: Int): List[((InstantiatedCPModel) => Unit,SubproblemData)] = {
    val l = decomposeToMap(model, count)
    l.map((m) => {
      ((instantiated_model: InstantiatedCPModel) => {
        for ((variable, value) <- m._1) {
          instantiated_model.post(variable == value)
        }
      }, m._2)
    })
  }
}

class DiscrepancyDecompositionStrategy(val sub: DecompositionStrategy) extends DecompositionStrategy {
  def decompose(model: UninstantiatedModel, count: Int): List[((InstantiatedCPModel) => Unit,SubproblemData)] = {
    sub.decompose(model, count).sortBy(_._2.discrepancy)
  }
}

class SubproblemData(val cartesianProductLog: Double, val minBound: Int, val discrepancy: Int)
{
  def this(cartesianProductLog: Double, optimisationMethod: OptimisationMethod, discrepancy: Int = -1) = this(cartesianProductLog,
    optimisationMethod match {
      case Maximisation(v) =>
        v.max
      case Minimisation(v) =>
        v.min
      case NoOptimisation() =>
        0
    }, discrepancy)

  def this(cartesianProductLog: Double, minBound: Int) = this(cartesianProductLog, minBound, -1)
}

class SubproblemQueue(orignal_list: List[((InstantiatedCPModel) => Unit, SubproblemData)], maximisation: Boolean) {
  val subproblems = orignal_list.toArray
  val todo = new LinkedBlockingQueue[Int]()
  for(sp <- orignal_list.indices)
    todo.add(sp)
  val pruned = Array.tabulate(subproblems.length)(i => false)
  var notPrunedNb = subproblems.length
  val done = Array.tabulate(subproblems.length)(i => false)
  var notDoneNb = subproblems.length

  def poll(): (Int, (InstantiatedCPModel) => Unit) = {
    val next = todo.poll()
    if(pruned(next))
      poll()
    else
      (next, subproblems(next)._1)
  }

  def updateBound(value: Int) = {
    for(i <- subproblems.indices) {
      if(!pruned(i) && !done(i) &&
        ((maximisation && subproblems(i)._2.minBound < value) || (!maximisation && subproblems(i)._2.minBound > value))) {
        pruned(i) = true
        done(i) = true
        notPrunedNb -= 1
        notDoneNb -= 1
      }
    }
  }

  def setDone(spid: Int, timeTaken: Double) = {
    assert(!done(spid))
    done(spid) = true
    notDoneNb -= 1
  }
}