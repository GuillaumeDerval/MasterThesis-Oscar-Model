package solvers.cp

import java.util.concurrent.LinkedBlockingQueue

import constraints.Constraint
import models.{Model, ModelDeclaration, SynchronizedIntBoundaryManager}
import oscar.algo.search.SearchStatistics
import solvers.cp.branchings.Branching

trait Watcher[RetVal] {
  def startedSubproblem(spid: Int): Unit
  def endedSubproblem(spid: Int, timeTaken: Double, currentBound: Option[Int], searchStats: SearchStatistics): Unit
  def newSolution(solution: RetVal, newBound: Option[Int]): Unit
  def allDone(): Unit
}

class SolvingMessage
case class SolutionMessage[RetVal](solution: RetVal) extends SolvingMessage
case class DoneMessage(spid: Int, timeTakenNS: Double, searchStats: SearchStatistics) extends SolvingMessage
case class StartedMessage(spid: Int) extends SolvingMessage
case class AllDoneMessage() extends SolvingMessage

class WatcherRunnable[RetVal](watchers: Iterable[Watcher[RetVal]], outputQueue: LinkedBlockingQueue[SolvingMessage],
                      boundaryManager: Option[SynchronizedIntBoundaryManager]) extends Runnable {
  override def run(): Unit = {
    var done = false
    while(!done) {
      outputQueue.take() match {
        case SolutionMessage(solution: RetVal) => watchers.foreach(_.newSolution(solution, boundaryManager.map(_.get_boundary())))
        case DoneMessage(spid, newtimeTaken, searchStats) => watchers.foreach(_.endedSubproblem(spid, newtimeTaken,
          boundaryManager.map(_.get_boundary()), searchStats))
        case StartedMessage(spid) => watchers.foreach(_.startedSubproblem(spid))
        case AllDoneMessage() =>
          done = true
          watchers.foreach(_.allDone())
      }
    }
  }
}

class StatisticsWatcher[RetVal] extends Watcher[RetVal] {
  var currentStatistics = new SearchStatistics(0, 0, 0, false, 0, 0, 0)
  def get = currentStatistics
  override def startedSubproblem(spid: Int): Unit = {}
  override def newSolution(solution: RetVal, newBound: Option[Int]): Unit = {
    currentStatistics = new SearchStatistics(nNodes = currentStatistics.nNodes,
      nFails = currentStatistics.nFails,
      time = currentStatistics.time,
      completed = false,
      timeInTrail = currentStatistics.timeInTrail,
      maxTrailSize = currentStatistics.maxTrailSize,
      nSols = currentStatistics.nSols+1
    )
  }
  override def allDone(): Unit = {
    currentStatistics = new SearchStatistics(nNodes = currentStatistics.nNodes,
      nFails = currentStatistics.nFails,
      time = currentStatistics.time,
      completed = true,
      timeInTrail = currentStatistics.timeInTrail,
      maxTrailSize = currentStatistics.maxTrailSize,
      nSols = currentStatistics.nSols
    )
  }
  override def endedSubproblem(spid: Int, timeTaken: Double, currentBound: Option[Int], searchStats: SearchStatistics): Unit = {
    currentStatistics = new SearchStatistics(nNodes = currentStatistics.nNodes+searchStats.nNodes,
      nFails = currentStatistics.nFails+searchStats.nFails,
      time = currentStatistics.time + searchStats.time,
      completed = false,
      timeInTrail = currentStatistics.timeInTrail+searchStats.timeInTrail,
      maxTrailSize = Math.max(currentStatistics.maxTrailSize, searchStats.maxTrailSize),
      nSols = currentStatistics.nSols
    )
  }
}

/**
  * Proxy most functions to an underlying model
  * @param md
  * @tparam CPModelType
  * @tparam Retval
  */
class ModelProxy[CPModelType <: CPSolve[Retval], Retval](md: ModelDeclaration with CPModelType) {
  implicit val modelDeclaration: ModelDeclaration with CPModelType = md

  def getDeclaredModel = modelDeclaration.getDeclaredModel
  def getCurrentModel = modelDeclaration.getCurrentModel

  def getSearch = md.getSearch
  def setSearch(b: Branching): Unit = md.setSearch(b)
  def setSearch(b: => Seq[oscar.algo.search.Alternative]): Unit = md.setSearch(b)
  def onSolution = md.onSolution
  def onSolution(s: => Retval): Unit = md.onSolution(s)
  def onSolution(o: Model => Retval): Unit = md.onSolution(o)

  def post(constraint: Constraint): Unit = modelDeclaration.post(constraint)
}