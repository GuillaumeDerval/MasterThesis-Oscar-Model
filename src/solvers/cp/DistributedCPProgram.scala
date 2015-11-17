package solvers.cp

import java.util

import solvers.cp.decompositions.DecompositionStrategy

import scala.util.Random
import java.util.concurrent.LinkedBlockingQueue

import models.instantiated.InstantiatedCPModel
import misc.ComputeTimeTaken.computeTimeTaken
import models.uninstantiated.UninstantiatedModel
import models.{Model, ModelDeclaration}
import vars.IntVar
import misc.TimeHelper._

/**
  * Created by dervalguillaume on 5/11/15.
  */
class DistributedCPProgram[RetVal](md: ModelDeclaration with DistributedCPSolve[RetVal] = new ModelDeclaration() with DistributedCPSolve[RetVal]) {
  implicit val program = this
  implicit val modelDeclaration = md
  var threadsToLaunch = Runtime.getRuntime.availableProcessors()/2
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

    val threads = Array.tabulate(threadsToLaunch)(i => new Thread(new Subsolver(model, queue, outputQueue)))
    val pb = SubproblemGraphicalProgressBar[RetVal](subproblems.size, threadsToLaunch)
    val watcher_thread = new Thread(new WatcherRunnable(pb, outputQueue))

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

  class WatcherRunnable(watcher: Watcher[RetVal], outputQueue: LinkedBlockingQueue[ThreadMessage]) extends Runnable {
    override def run(): Unit = {
      var done = false
      while(!done) {
        outputQueue.take() match {
          case SolutionMessage(solution) => watcher.newSolution(solution)
          case DoneMessage(spid, newtimeTaken) => watcher.endedSubproblem(spid, newtimeTaken)
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
                  outputQueue: util.Queue[ThreadMessage]) extends Runnable {
    override def run(): Unit = {
      val cpmodel = new InstantiatedCPModel(uninstantiatedModel)
      modelDeclaration.applyFuncOnModel(cpmodel) {
        cpmodel.cpSolver.onSolution {
          outputQueue.add(SolutionMessage(onSolution(cpmodel)))
        }
        cpmodel.cpSolver.search(getSearch(cpmodel))

        while (!subproblemQueue.isEmpty) {
          val t0 = getThreadCpuTime
          val (sp, spid) = subproblemQueue.poll()
          outputQueue.add(StartedMessage(spid))
          cpmodel.cpSolver.startSubjectTo() {
            for ((variable, value) <- sp) {
              variable.assign(value)
            }
          }
          val t1 = getThreadCpuTime
          outputQueue.add(DoneMessage(spid, t1-t0))
        }
      }
    }
  }
}

trait Watcher[RetVal] {
  def startedSubproblem(spid: Int): Unit
  def endedSubproblem(spid: Int, timeTaken: Double): Unit
  def newSolution(solution: RetVal): Unit
  def allDone(): Unit
}