package solvers.cp.decompositions

import java.util.concurrent.LinkedBlockingQueue

import constraints.Constraint
import models.{CPModel, UninstantiatedModel}
import solvers.cp.SubproblemData

/**
  * Decomposition strategy that uses assignations as decompositions
  * Simple to share among multiple machines
  */
trait DecompositionStrategy extends Serializable {
  /**
    * Decompose the problem
    *
    * @param model the model to decompose
    * @param count the (minimum) number of subproblems wanted
    * @return A list of assignation to variable that makes the subproblem, along with the associated SubproblemData
    */
  def decompose(model: UninstantiatedModel, count: Int): List[(List[Constraint],SubproblemData)]
}


/**
  * Decomposition strategy that uses closures at decompositions;
  * can only be used in the same JVM, so only for locally parallelized processes
  */
trait ClosureDecompositionStrategy {
  /**
    * Decompose the problem
    *
    * @param model the model to decompose
    * @param count the (minimum) number of subproblems wanted
    * @return A list of closure (to be applied on a child model) that gives
    *         the wanted subproblem, and the SubproblemData object associated
    */
  def decompose(model: UninstantiatedModel, count: Int): List[((CPModel) => Unit,SubproblemData)]
}


/**
  * Convert a DecompositionStrategy to a ClosureDecompositionStrategy
  *
  * @param sub the DecompositionStrategy to transform
  */
class DecompositionStrategyToClosureConverter(sub: DecompositionStrategy) extends ClosureDecompositionStrategy{
  def decompose(model: UninstantiatedModel, count: Int): List[((CPModel) => Unit,SubproblemData)] = {
    val l = sub.decompose(model, count)
    l.map((m) => {
      ((instantiated_model: CPModel) => {
        for (constraint <- m._1)
          instantiated_model.post(constraint)
      }, m._2)
    })
  }
}

/**
  * An object that contains an implicit that converts from DecompositionStrategy to ClosureDecompositionStrategy
  */
object DecompositionStrategyToClosureConverter {
  implicit def convert(sub: DecompositionStrategy): ClosureDecompositionStrategy = new DecompositionStrategyToClosureConverter(sub)
}

class SubproblemQueue(orignal_list: List[((CPModel) => Unit, SubproblemData)], maximisation: Boolean) {
  val subproblems = orignal_list.toArray
  val todo = new LinkedBlockingQueue[Int]()
  for(sp <- orignal_list.indices)
    todo.add(sp)
  val pruned = Array.tabulate(subproblems.length)(i => false)
  var notPrunedNb = subproblems.length
  val done = Array.tabulate(subproblems.length)(i => false)
  var notDoneNb = subproblems.length

  def poll(): (Int, (CPModel) => Unit) = {
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