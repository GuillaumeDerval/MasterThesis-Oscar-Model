package solvers.cp

import java.util

import constraints.Constraint
import solvers.cp.decompositions.DecompositionStrategy

import scala.util.Random
import java.util.concurrent.LinkedBlockingQueue

import models.instantiated.InstantiatedCPModel
import misc.ComputeTimeTaken.computeTimeTaken
import models.uninstantiated.UninstantiatedModel
import models._
import vars.IntVar
import misc.TimeHelper._

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
    * @param constraint
    */
  def post(constraint: Constraint): Unit = modelDeclaration.post(constraint)

  def solve(): Unit = solve(modelDeclaration.getCurrentModel)

  def solve(model: Model): Unit = {
    model match {
      case m: UninstantiatedModel => solve(m)
      case _ => sys.error("The model is already instantiated")
    }
  }

  def solve(model: UninstantiatedModel): Unit = {

    val subproblems = computeTimeTaken("Decomposition"){Random.shuffle(getDecompositionStrategy.decompose(model, subproblemsCount))}

    val queue = new LinkedBlockingQueue[(Map[IntVar, Int], Int)]()
    val outputQueue = new LinkedBlockingQueue[ThreadMessage]()

    for (s <- subproblems.zipWithIndex)
      queue.add(s)

    val boundaryManager:Option[SynchronizedIntBoundaryManager] = model.optimisationMethod match {
      case m: Minimisation => Some(new SynchronizedIntBoundaryManager(m.objective.max))
      case m: Maximisation => Some(new SynchronizedIntBoundaryManager(m.objective.min))
      case _ => None
    }

    val threads = Array.tabulate(threadsToLaunch)(i => new Thread(new Subsolver(model, queue, outputQueue, boundaryManager)))
    val pb = SubproblemGraphicalProgressBar[RetVal](subproblems.size, threadsToLaunch)
    val watcher_thread = new Thread(new WatcherRunnable(pb, outputQueue, boundaryManager))

    watcher_thread.start()
    threads.foreach(_.start())
    pb.start()
    threads.foreach(_.join())
    outputQueue.add(AllDoneMessage())
    watcher_thread.join()
  }

  class ThreadMessage
  case class SolutionMessage(solution: RetVal) extends ThreadMessage
  case class DoneMessage(spid: Int, timeTakenNS: Double) extends ThreadMessage
  case class StartedMessage(spid: Int) extends ThreadMessage
  case class AllDoneMessage() extends ThreadMessage

  class WatcherRunnable(watcher: Watcher[RetVal], outputQueue: LinkedBlockingQueue[ThreadMessage],
                        boundaryManager: Option[SynchronizedIntBoundaryManager]) extends Runnable {
    override def run(): Unit = {
      var done = false
      while(!done) {
        outputQueue.take() match {
          case SolutionMessage(solution) => watcher.newSolution(solution)
          case DoneMessage(spid, newtimeTaken) => watcher.endedSubproblem(spid, newtimeTaken,
            boundaryManager.map(_.get_boundary()))
          case StartedMessage(spid) => watcher.startedSubproblem(spid)
          case AllDoneMessage() =>
            done = true
            watcher.allDone()
        }
      }
    }
  }

  class Subsolver(uninstantiatedModel: UninstantiatedModel,
                  subproblemQueue: util.Queue[(Map[IntVar, Int], Int)],
                  outputQueue: util.Queue[ThreadMessage],
                  boundaryManager: Option[SynchronizedIntBoundaryManager]) extends Runnable {
    override def run(): Unit = {
      val cpmodel = new InstantiatedCPModel(uninstantiatedModel)
      modelDeclaration.applyFuncOnModel(cpmodel) {
        //if(onSolution != null)
        //  cpmodel.cpSolver.onSolution {}
        //else
        //  cpmodel.cpSolver.onSolution {}

        val objv: IntVar = cpmodel.optimisationMethod match {
          case m: Minimisation => m.objective
          case m: Maximisation => m.objective
          case _ => null
        }

        //val initialOnSolution: (Model => Unit) = (a) => outputQueue.add(SolutionMessage(onSolution(cpmodel)))
        val initialOnSolution: (Model => Unit) = (a) => {}

        val solution: Model => Unit = boundaryManager match {
          case Some(bm) => CPIntBoundaryUpdateSolutionWrapper(initialOnSolution, bm, objv)
          case None => initialOnSolution
        }

        val search: oscar.algo.search.Branching =boundaryManager match {
          case Some(bm) => new IntBoundaryUpdateSearchWrapper(getSearch(cpmodel), bm, cpmodel.cpObjective)
          case None => getSearch(cpmodel)
        }

        cpmodel.cpSolver.onSolution { solution(cpmodel) }

        cpmodel.cpSolver.search(search)

        while (!subproblemQueue.isEmpty) {
          val t0 = getThreadCpuTime
          val todo = subproblemQueue.poll()
          if(todo != null) {
            val (sp, spid) = todo
            outputQueue.add(StartedMessage(spid))
            cpmodel.cpSolver.startSubjectTo() {
              for ((variable, value) <- sp) {
                post(variable == value)
              }
            }
            val t1 = getThreadCpuTime
            outputQueue.add(DoneMessage(spid, t1-t0))
          }
        }
      }
    }
  }
}

trait Watcher[RetVal] {
  def startedSubproblem(spid: Int): Unit
  def endedSubproblem(spid: Int, timeTaken: Double, currentBound: Option[Int]): Unit
  def newSolution(solution: RetVal): Unit
  def allDone(): Unit
}