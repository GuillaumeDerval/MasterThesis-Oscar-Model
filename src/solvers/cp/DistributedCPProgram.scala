package solvers.cp

import java.util

import constraints.Constraint
import oscar.cp.TightenType
import solvers.cp.decompositions.{DecompositionStrategy, SubproblemData}

import scala.util.Random
import java.util.concurrent.LinkedBlockingQueue

import models.instantiated.InstantiatedCPModel
import misc.ComputeTimeTaken.computeTimeTaken
import models.uninstantiated.{ChildModel, UninstantiatedModel}
import models._
import vars.IntVar
import misc.TimeHelper._
import oscar.algo.search.SearchStatistics

/**
  * Created by dervalguillaume on 5/11/15.
  */
class DistributedCPProgram[RetVal](md: ModelDeclaration with DistributedCPSolve[RetVal] = new ModelDeclaration() with DistributedCPSolve[RetVal]) {
  implicit val program = this
  implicit val modelDeclaration = md
  var threadsToLaunch = Runtime.getRuntime.availableProcessors()
  var subproblemsCount = 100*threadsToLaunch

  def getDeclaredModel = modelDeclaration.getDeclaredModel

  def getCurrentModel = modelDeclaration.getCurrentModel

  def setDecompositionStrategy(d: DecompositionStrategy): Unit = md.setDecompositionStrategy(d)
  def getDecompositionStrategy: DecompositionStrategy = md.getDecompositionStrategy

  def getSearch = md.getSearch

  def setSearch(b: Branching): Unit = md.setSearch(b)

  def setSearch(b: => Seq[oscar.algo.search.Alternative]): Unit = md.setSearch(b)

  def onSolution = md.onSolution

  def onSolution(s: => RetVal): Unit = md.onSolution(s)

  def onSolution(o: Model => RetVal): Unit = md.onSolution(o)

  /**
    * Post a new constraint
 *
    * @param constraint
    */
  def post(constraint: Constraint): Unit = modelDeclaration.post(constraint)

  def solve(): SearchStatistics = solve(modelDeclaration.getCurrentModel)

  def solve(model: Model): SearchStatistics = {
    model match {
      case m: UninstantiatedModel => solve(m)
      case _ => sys.error("The model is already instantiated")
    }
  }

  def solve(model: UninstantiatedModel): SearchStatistics = {

    val subproblems = computeTimeTaken("Decomposition"){/*Random.shuffle(*/getDecompositionStrategy.decompose(model, subproblemsCount)/*)*/}

    println("Subproblems: "+subproblems.length.toString)

    val queue = new LinkedBlockingQueue[((InstantiatedCPModel) => Unit, Int)]()
    val outputQueue = new LinkedBlockingQueue[ThreadMessage]()

    for (s <- subproblems.zipWithIndex)
      queue.add((s._1._1, s._2))

    val boundaryManager:Option[SynchronizedIntBoundaryManager] = model.optimisationMethod match {
      case m: Minimisation => Some(new SynchronizedIntBoundaryManager(m.objective.max))
      case m: Maximisation => Some(new SynchronizedIntBoundaryManager(m.objective.min))
      case _ => None
    }

    val threads = Array.tabulate(threadsToLaunch)(i => new Thread(new Subsolver(model, queue, outputQueue, boundaryManager)))
    val pb = SubproblemGraphicalProgressBar[RetVal](subproblems.size, threadsToLaunch)
    pb.setSubproblemsData(subproblems.zipWithIndex.map(m => (m._2, m._1._2)))

    val statWatcher = new StatisticsWatcher
    val watchers = Array[Watcher[RetVal]](statWatcher, pb)
    val watcher_thread = new Thread(new WatcherRunnable(watchers, outputQueue, boundaryManager))

    watcher_thread.start()
    threads.foreach(_.start())
    pb.start()
    threads.foreach(_.join())
    outputQueue.add(AllDoneMessage())
    watcher_thread.join()
    statWatcher.get
  }

  class ThreadMessage
  case class SolutionMessage(solution: RetVal) extends ThreadMessage
  case class DoneMessage(spid: Int, timeTakenNS: Double, searchStats: SearchStatistics) extends ThreadMessage
  case class StartedMessage(spid: Int) extends ThreadMessage
  case class AllDoneMessage() extends ThreadMessage

  class WatcherRunnable(watchers: Iterable[Watcher[RetVal]], outputQueue: LinkedBlockingQueue[ThreadMessage],
                        boundaryManager: Option[SynchronizedIntBoundaryManager]) extends Runnable {
    override def run(): Unit = {
      var done = false
      while(!done) {
        outputQueue.take() match {
          case SolutionMessage(solution) => watchers.foreach(_.newSolution(solution, boundaryManager.map(_.get_boundary())))
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

  class StatisticsWatcher extends Watcher[RetVal] {
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

  class Subsolver(uninstantiatedModel: UninstantiatedModel,
                  subproblemQueue: util.Queue[((InstantiatedCPModel) => Unit, Int)],
                  outputQueue: util.Queue[ThreadMessage],
                  boundaryManager: Option[SynchronizedIntBoundaryManager]) extends Runnable {
    override def run(): Unit = {
      val cpmodel = new InstantiatedCPModel(uninstantiatedModel)
      modelDeclaration.applyFuncOnModel(cpmodel) {
        cpmodel.cpSolver.silent = true

        val objv: IntVar = cpmodel.optimisationMethod match {
          case m: Minimisation => m.objective
          case m: Maximisation => m.objective
          case _ => null
        }

        val initialOnSolution: (Model => Unit) = (a) => outputQueue.add(SolutionMessage(onSolution(cpmodel)))
        //val initialOnSolution: (Model => Unit) = (a) => {}

        val solution: Model => Unit = boundaryManager match {
          case Some(bm) => CPIntBoundaryUpdateSolutionWrapper(initialOnSolution, bm, objv)
          case None => initialOnSolution
        }

        //TODO: test without this
        val search: oscar.algo.search.Branching = boundaryManager match {
          case Some(bm) => new IntBoundaryUpdateSearchWrapper(getSearch(cpmodel), bm, cpmodel.cpObjective)
          case None => getSearch(cpmodel)
        }
        //val search: oscar.algo.search.Branching = getSearch(cpmodel)


        while (!subproblemQueue.isEmpty) {
          val t0 = getThreadCpuTime
          val todo = subproblemQueue.poll()
          if(todo != null) {
            val (sp, spid) = todo
            outputQueue.add(StartedMessage(spid))
            val info = cpmodel.cpSolver.startSubjectTo() {
              cpmodel.cpObjective.tightenMode = TightenType.NoTighten
              sp(cpmodel)
              cpmodel.cpObjective.tightenMode = TightenType.StrongTighten
              /*
               * Note: this has to be made after the call to the selection function, because it may overwrite the search
               * and the solution handling function AND may want to use the original status at decomposition
               */
              cpmodel.cpSolver.searchEngine.clearOnSolution()
              cpmodel.cpSolver.onSolution { solution(cpmodel) }
              cpmodel.cpSolver.search(search)

              /*
               * Set the current bound at start
               */
              boundaryManager match {
                case Some(bm) => {
                  cpmodel.cpObjective.updateWorstBound(bm.get_boundary())
                  cpmodel.cpObjective.best = bm.get_boundary()
                }
                case None =>
              }
            }
            cpmodel.cpSolver.searchEngine.clearOnSolution()
            val t1 = getThreadCpuTime
            outputQueue.add(DoneMessage(spid, t1-t0, info))
          }
        }
      }
    }
  }
}

trait Watcher[RetVal] {
  def startedSubproblem(spid: Int): Unit
  def endedSubproblem(spid: Int, timeTaken: Double, currentBound: Option[Int], searchStats: SearchStatistics): Unit
  def newSolution(solution: RetVal, newBound: Option[Int]): Unit
  def allDone(): Unit
}