package solvers.cp

import java.util.concurrent.LinkedBlockingQueue

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.Logging
import akka.routing.{ActorRefRoutee, BroadcastRoutingLogic, Router}
import com.typesafe.config.ConfigFactory
import misc.ComputeTimeTaken._
import misc.TimeHelper._
import models._
import models.instantiated.InstantiatedCPModel
import models.uninstantiated.UninstantiatedModel
import oscar.algo.search.SearchStatistics
import oscar.cp.core.CPPropagStrength
import oscar.cp.{CPIntVar, TightenType}
import solvers.cp.decompositions.DecompositionStrategy
import vars.IntVar

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
  * A CPProgram that can distribute works among a cluster
  *
  * @param md
  * @tparam RetVal
  */
class DistributedCPProgram[RetVal](md: ModelDeclaration with DecomposedCPSolve[RetVal] = new ModelDeclaration() with DecomposedCPSolve[RetVal])
  extends ModelProxy[DecomposedCPSolve[RetVal], RetVal](md) {

  var subproblemsPerWorker = 250

  implicit val program = this

  def setDecompositionStrategy(d: DecompositionStrategy): Unit = md.setDecompositionStrategy(d)

  def getDecompositionStrategy: DecompositionStrategy = md.getDecompositionStrategy

  def solveLocally(threadCount: Int = Runtime.getRuntime.availableProcessors()): SearchStatistics = solveLocally(modelDeclaration.getCurrentModel, threadCount)

  def solveLocally(model: Model, threadCount: Int): SearchStatistics = {
    model match {
      case m: UninstantiatedModel => solveLocally(m, threadCount)
      case _ => sys.error("The model is already instantiated")
    }
  }

  def solve(model: UninstantiatedModel, subproblemCount: Int, createSolvers: (ActorSystem, ActorRef) => Unit): SearchStatistics = {
    val subproblems = computeTimeTaken("Decomposition") {
      getDecompositionStrategy.decompose(model, subproblemCount)
    }
    println("Subproblems: " + subproblems.length.toString)

    val queue = new LinkedBlockingQueue[(Int, Map[Int, Int])]()
    val outputQueue = new LinkedBlockingQueue[SolvingMessage]()

    for (s <- subproblems.zipWithIndex)
      queue.add((s._2, s._1._1.map(
        { case (a, b) => (a.varid, b) }
      )))

    val pb = SubproblemGraphicalProgressBar[RetVal](subproblems.size, 0)
    pb.setSubproblemsData(subproblems.zipWithIndex.map(m => (m._2, m._1._2)))

    val statWatcher = new StatisticsWatcher[RetVal]
    val watchers = Array[Watcher[RetVal]](statWatcher, pb)
    val watcher_thread = new Thread(new WatcherRunnable(watchers, outputQueue))
    watcher_thread.start()

    val system = ActorSystem("ClusterSystem")
    val masterActor = system.actorOf(Props(new SolverMaster(md, model, queue, outputQueue)), "master")

    createSolvers(system, masterActor)

    pb.start()

    Await.result(system.whenTerminated, Duration.Inf)
    watcher_thread.join()

    statWatcher.get
  }

  def solveLocally(model: UninstantiatedModel, threadCount: Int): SearchStatistics = {
    solve(model, subproblemsPerWorker*threadCount, (system, masterActor) => {
      for(i <- 0 until threadCount)
        system.actorOf(SolverActor.props[RetVal](md, model, masterActor))
    })
  }
}

/**
  * An actor that manages a collection of SolverActor
  * @param modelDeclaration
  * @param uninstantiatedModel
  * @param subproblemQueue
  * @param outputQueue
  * @tparam RetVal
  */
class SolverMaster[RetVal](modelDeclaration: ModelDeclaration with DecomposedCPSolve[RetVal],
                           uninstantiatedModel: UninstantiatedModel,
                           subproblemQueue: LinkedBlockingQueue[(Int, Map[Int, Int])],
                           outputQueue: LinkedBlockingQueue[SolvingMessage]) extends Actor with IntBoundaryManager {
  val log = Logging(context.system, this)
  var done = false

  @volatile private var boundary: Int = uninstantiatedModel.optimisationMethod match {
    case m: Minimisation => uninstantiatedModel.getRepresentative(m.objective).max
    case m: Maximisation => uninstantiatedModel.getRepresentative(m.objective).min
    case _               => 0
  }

  def get_boundary(): Int = boundary

  def update_boundary(newval: Int) = uninstantiatedModel.optimisationMethod match {
    case m: Minimisation =>
      if(boundary > newval)
        boundary = newval
    case m: Maximisation =>
      if(boundary < newval)
        boundary = newval
  }

  var broadcastRouter = Router(BroadcastRoutingLogic())

  /**
    * Process messages from master
    */
  def receive = {
    case AwaitingSPMessage() =>
      context watch sender
      broadcastRouter = broadcastRouter.addRoutee(sender)
      context.sender() ! BoundUpdateMessage(get_boundary()) //ensure the new member has the last bound
      sendNextJob(sender)
    case a: DoneMessage =>
      sendNextJob(sender)
      outputQueue.add(a)
    case SolutionMessage(solution, Some(b)) =>
      broadcastRouter.route(BoundUpdateMessage(b), self)
      outputQueue.add(SolutionMessage(solution, Some(b)))
    case a: WatcherMessage => outputQueue.add(a)
    case _ => log.info("received unknown message")
  }

  /**
    * Send a new sp to a given actor. If there is no subproblem, broadcast a AllDone message to everyone
    *
    * @param to
    */
  def sendNextJob(to: ActorRef): Unit = {
    if (subproblemQueue.isEmpty) {
      if (!done) {
        broadcastRouter.route(AllDoneMessage(), self)
        context.system.terminate()
        done = true
      }
    }
    else {
      val next = subproblemQueue.poll()
      to ! DoSubproblemMessage(next._1, next._2)
    }
  }
}

/**
  * A solver actor, that solves one subproblem at once
  *
  * @param modelDeclaration
  * @param uninstantiatedModel
  * @tparam RetVal
  */
class SolverActor[RetVal](modelDeclaration: ModelDeclaration with DecomposedCPSolve[RetVal],
                          uninstantiatedModel: UninstantiatedModel,
                          master: ActorRef) extends Actor with IntBoundaryManager {
  val log = Logging(context.system, this)

  import context.dispatcher

  val cpmodel = new InstantiatedCPModel(uninstantiatedModel)
  cpmodel.cpSolver.silent = true

  @volatile private var boundary = 0

  val objv: IntVar = cpmodel.optimisationMethod match {
    case m: Minimisation =>
      boundary = cpmodel.getRepresentative(m.objective).max
      m.objective
    case m: Maximisation =>
      boundary = cpmodel.getRepresentative(m.objective).min
      m.objective
    case _ => null
  }

  val solution: Model => Unit = cpmodel.optimisationMethod match {
    case m: Minimisation =>
      log.info("MIN")
      (a) => {
        val v = cpmodel.getRepresentative(objv)
        this.update_boundary(v.max)
        master ! SolutionMessage(modelDeclaration.onSolution(cpmodel), Some(v.max))
      }
    case m: Maximisation =>
      log.info("MAX")
      (a) => {
        val v = cpmodel.getRepresentative(objv)
        this.update_boundary(v.max)
        master ! SolutionMessage(modelDeclaration.onSolution(cpmodel), Some(v.max))

      }
    case _ => (a) => master ! SolutionMessage(modelDeclaration.onSolution(cpmodel), None)
  }

  val search: oscar.algo.search.Branching = objv match {
    case null => modelDeclaration.getSearch(cpmodel)
    case _ => new IntBoundaryUpdateSearchWrapper(modelDeclaration.getSearch(cpmodel), this, cpmodel.cpObjective)
  }
  //val search: oscar.algo.search.Branching = getSearch(cpmodel)

  // Tell our master that we are waiting for a subproblem
  master ! AwaitingSPMessage()

  /**
    * Process messages from master
    */
  def receive = {
    case DoSubproblemMessage(spid: Int, sp: Map[Int, Int]) => {
      log.info("received subproblem")
      Future {
        solve_subproblem(spid, sp)
      }
    }
    case BoundUpdateMessage(newBound: Int) => {
      this.update_boundary(newBound)
    }
    case AllDoneMessage() => {
      context.stop(self)
    }
    case _ => log.info("received unknown message")
  }

  /**
    * Solve the current subproblem; should be called from a Future.
    */
  def solve_subproblem(spid: Int, sp: Map[Int, Int]): Unit = {
    val t0 = getThreadCpuTime
    val info = modelDeclaration.applyFuncOnModel(cpmodel) {
      val v = cpmodel.cpSolver.startSubjectTo() {

        cpmodel.cpObjective.tightenMode = TightenType.NoTighten
        for ((variable, value) <- sp) {
          val v: CPIntVar = cpmodel.intRepresentatives.find(variable).realCPVar
          cpmodel.cpSolver.post(v == value, CPPropagStrength.Strong)
        }
        cpmodel.cpObjective.tightenMode = TightenType.StrongTighten

        /*
         * Note: this has to be made after the call to the selection function, because it may overwrite the search
         * and the solution handling function AND may want to use the original status at decomposition
         */
        cpmodel.cpSolver.searchEngine.clearOnSolution()
        cpmodel.cpSolver.onSolution {
          solution(cpmodel)
        }
        cpmodel.cpSolver.search(search)

        /*
         * Set the current bound at start
         */
        if (null != objv) {
          cpmodel.cpObjective.updateWorstBound(get_boundary())
          cpmodel.cpObjective.best = get_boundary()
        }
      }
      cpmodel.cpSolver.searchEngine.clearOnSolution()
      v
    }
    val t1 = getThreadCpuTime
    master ! DoneMessage(spid, t1 - t0, info)
  }

  def get_boundary(): Int = boundary

  def update_boundary(newval: Int) = boundary = newval
}

object SolverActor {
  def props[RetVal](modelDeclaration: ModelDeclaration with DecomposedCPSolve[RetVal], uninstantiatedModel: UninstantiatedModel, master: ActorRef): Props =
    Props(new SolverActor[RetVal](modelDeclaration, uninstantiatedModel, master))
}