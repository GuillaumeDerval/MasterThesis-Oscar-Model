package solvers.cp

import java.util.concurrent.LinkedBlockingQueue

import akka.actor._
import akka.event.Logging
import akka.pattern.ask
import akka.remote.RemoteScope
import akka.routing.{BroadcastRoutingLogic, Router}
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import misc.ComputeTimeTaken._
import misc.SearchStatistics
import misc.TimeHelper._
import models._
import oscar.cp.core.CPPropagStrength
import oscar.cp.{CPIntVar, TightenType}
import solvers.cp.decompositions.DecompositionStrategy
import vars.IntVar

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.{Duration, _}
import scala.concurrent.{Await, Future}

/**
  * A CPProgram that can distribute works among a cluster
  *
  * @param md
  * @tparam RetVal
  */
class DistributedCPProgram[RetVal](md: ModelDeclaration with DecomposedCPSolve[RetVal] = new ModelDeclaration() with DecomposedCPSolve[RetVal])
  extends ModelProxy[DecomposedCPSolve[RetVal], RetVal](md) {
  implicit val program = this

  protected val registeredWatchers: scala.collection.mutable.ListBuffer[(
      (List[(Map[IntVar, Int], SubproblemData)]) => Watcher[RetVal],
      (Watcher[RetVal]) => Unit
    )] = ListBuffer()

  /**
    * Register a new watcher to be added when the solving begins
    *
    * @param creator creates a new Watcher, given the subproblem list
    * @param initiator initiate the Watcher. Called after the resolution has begun.
    */
  def registerWatcher(creator: (List[(Map[IntVar, Int], SubproblemData)]) => Watcher[RetVal],
                      initiator: (Watcher[RetVal]) => Unit): Unit = {
    registeredWatchers += ((creator, initiator))
  }

  def setDecompositionStrategy(d: DecompositionStrategy): Unit = md.setDecompositionStrategy(d)

  def getDecompositionStrategy: DecompositionStrategy = md.getDecompositionStrategy

  /**
    * Starts the CPProgram locally on threadCount threads, on the current model
    *
    * @param threadCount number of threads to use. By default, it is the number of available CPU
    * @return
    */
  def solveLocally(threadCount: Int = Runtime.getRuntime.availableProcessors(), sppw: Int = 100): (SearchStatistics, List[RetVal]) = solveLocally(modelDeclaration.getCurrentModel, threadCount, sppw)

  /**
    * Starts the CPProgram locally on threadCount threads, on the current model
    *
    * @param model the model to solve
    * @param threadCount number of threads to use
    * @return
    */
  def solveLocally(model: Model, threadCount: Int, sppw: Int): (SearchStatistics, List[RetVal]) = {
    model match {
      case m: UninstantiatedModel => solveLocally(m, threadCount, sppw)
      case _ => sys.error("The model is already instantiated")
    }
  }

  /**
    * Starts the CPProgram locally on threadCount threads, on the current model
    *
    * @param model the model to solve
    * @param threadCount number of threads to use
    * @return
    */
  def solveLocally(model: UninstantiatedModel, threadCount: Int, sppw: Int): (SearchStatistics, List[RetVal]) = {
    solve(model, sppw*threadCount,
      AkkaConfigCreator.local(),
      (system, masterActor) => List.fill(threadCount)(system.actorOf(SolverActor.props[RetVal](md, masterActor)))
    )
  }

  /**
    * Starts the CPProgram in a distributed fashion
    *
    * @param remoteHosts list of tuples (hostname, port) on which remote Akka ActorSystems can be contacted
    * @param localhost tupe (hostname, port) on which the local ActorSystem will be contacted by remote Actors
    * @return
    */
  def solveDistributed(remoteHosts: List[(String, Int)], localhost: (String, Int), sppw: Int = 100): (SearchStatistics, List[RetVal]) = {
    solveDistributed(modelDeclaration.getCurrentModel, remoteHosts, localhost, sppw)
  }

  /**
    * Starts the CPProgram in a distributed fashion
    *
    * @param model model to solve
    * @param remoteHosts list of tuples (hostname, port) on which remote Akka ActorSystems can be contacted
    * @param localhost tupe (hostname, port) on which the local ActorSystem will be contacted by remote Actors
    * @return
    */
  def solveDistributed(model: Model, remoteHosts: List[(String, Int)], localhost: (String, Int), sppw: Int): (SearchStatistics, List[RetVal]) = {
    model match {
      case m: UninstantiatedModel => solveDistributed(m, remoteHosts, localhost, sppw)
      case _ => sys.error("The model is already instantiated")
    }
  }

  /**
    * Starts the CPProgram in a distributed fashion
    *
    * @param model model to solve
    * @param remoteHosts list of tuples (hostname, port) on which remote Akka ActorSystems can be contacted
    * @param localhost tupe (hostname, port) on which the local ActorSystem will be contacted by remote Actors
    * @return
    */
  def solveDistributed(model: UninstantiatedModel, remoteHosts: List[(String, Int)], localhost: (String, Int), sppw: Int): (SearchStatistics, List[RetVal]) = {
    val (hostname, port) = localhost
    val config = AkkaConfigCreator.remote(hostname, port)

    solve(model, sppw*remoteHosts.length, config,
      (system, masterActor) => {
        remoteHosts.map(t => {
          val (hostnameL, portL) = t
          val address = Address("akka.tcp", "solving", hostnameL, portL)
          system.actorOf(SolverActor.props[RetVal](md, masterActor).withDeploy(Deploy(scope = RemoteScope(address))))
        })
      }
    )
  }

  /**
    * Starts the CPProgram using possibly remote solvers, created by createSolvers
    *
    * @param model model to solve
    * @param subproblemCount number of subproblems needed
    * @param systemConfig akka config to use
    * @param createSolvers function that creates SolverActor
    * @return
    */
  def solve(model: UninstantiatedModel, subproblemCount: Int, systemConfig: Config, createSolvers: (ActorSystem, ActorRef) => List[ActorRef]): (SearchStatistics, List[RetVal]) = {
    modelDeclaration.apply(model) {
      val subproblems: List[(Map[IntVar, Int], SubproblemData)] = computeTimeTaken("decomposition", "solving") {
        getDecompositionStrategy.decompose(model, subproblemCount)
      }
      println("Subproblems: " + subproblems.length.toString)

      val queue = new LinkedBlockingQueue[(Int, Map[Int, Int])]()
      val outputQueue = new LinkedBlockingQueue[SolvingMessage]()

      for (s <- subproblems.zipWithIndex)
        queue.add((s._2, s._1._1.map(
          { case (a, b) => (a.varid, b) }
        )))

      //Create watchers
      val createdWatchers = registeredWatchers.map((tuple) => {
        val watcher = tuple._1(subproblems)
        (watcher, () => tuple._2(watcher))
      })

      val statWatcher = new StatisticsWatcher[RetVal]
      val watchers = createdWatchers.map(_._1).toArray ++ Array[Watcher[RetVal]](statWatcher)
      val watcher_thread = new Thread(new WatcherRunnable(watchers, outputQueue))
      watcher_thread.start()

      val (system, masterActor, subsolvers) = computeTimeTaken("actor creation", "network") {
        val system = ActorSystem("solving", systemConfig)
        val masterActor = system.actorOf(Props(new SolverMaster(md, queue, outputQueue)), "master")

        // Create solvers and wait for them to be started
        val subsolvers = createSolvers(system, masterActor)
        implicit val timeout = Timeout(2 minutes)
        subsolvers.map((a) => a ? HelloMessage()).map((f) => Await.result(f, Duration.Inf))
        (system, masterActor, subsolvers)
      }

      for(tuple <- createdWatchers)
        tuple._2()

      // Start solving
      computeTimeTaken("solving", "solving") {
        // TODO this maybe should be in SolverMaster
        subsolvers.foreach((a) => a ! StartMessage())

        Await.result(system.whenTerminated, Duration.Inf)
      }
      watcher_thread.join()

      showSummary()
      statWatcher.get
    }
  }
}

class SimpleRemoteSolverSystem(hostname: String, port: Int) {
  val system = ActorSystem("solving", AkkaConfigCreator.remote(hostname, port))
  Await.result(system.whenTerminated, Duration.Inf)
}

private object AkkaConfigCreator {
  def remote(hostname: String, port: Int): Config = {
    ConfigFactory.parseString(s"""
       akka {
         actor {
           provider = "akka.remote.RemoteActorRefProvider"
           serializers {
             java = "akka.serialization.JavaSerializer"
             kryo = "com.twitter.chill.akka.AkkaSerializer"
           }
           serialization-bindings {
             "solvers.cp.SolvingMessage" = java
             "models.Model" = java
             "models.ModelDeclaration" = kryo
           }
         }
         remote {
           enabled-transports = ["akka.remote.netty.tcp"]
           netty.tcp {
             hostname = "$hostname"
             port = $port
           }
         }
       }
     """)
  }

  def local(): Config = {
    ConfigFactory.load()
    //ConfigFactory.parseString(s"""akka { loglevel = DEBUG }""")
  }
}

/**
  * An actor that manages a collection of SolverActor
  *
  * @param modelDeclaration that contains an UninstantiatedModel as current model (the one to be solved)
  * @param subproblemQueue
  * @param outputQueue
  * @tparam RetVal
  */
class SolverMaster[RetVal](modelDeclaration: ModelDeclaration with DecomposedCPSolve[RetVal],
                           subproblemQueue: LinkedBlockingQueue[(Int, Map[Int, Int])],
                           outputQueue: LinkedBlockingQueue[SolvingMessage]) extends Actor with IntBoundaryManager {
  val log = Logging(context.system, this)
  var done = false

  val uninstantiatedModel = modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel]

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
      outputQueue.add(a)
      sendNextJob(sender)

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
        outputQueue.add(AllDoneMessage())
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
  * @param modelDeclaration that contains an UninstantiatedModel as current model (this is the one to be solved)
  * @tparam RetVal
  */
class SolverActor[RetVal](modelDeclaration: ModelDeclaration with DecomposedCPSolve[RetVal],
                          master: ActorRef) extends Actor with IntBoundaryManager {
  val log = Logging(context.system, this)

  import context.dispatcher

  val uninstantiatedModel = modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel]

  val cpmodel = new CPModel(uninstantiatedModel)
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

  val on_solution: () => RetVal = modelDeclaration.onSolution

  val solution: Model => Unit = cpmodel.optimisationMethod match {
    case m: Minimisation =>
      log.info("MIN")
      (a) => {
        val v = cpmodel.getRepresentative(objv)
        this.update_boundary(v.max)
        master ! SolutionMessage(on_solution(), Some(v.max))
      }
    case m: Maximisation =>
      log.info("MAX")
      (a) => {
        val v = cpmodel.getRepresentative(objv)
        this.update_boundary(v.max)
        master ! SolutionMessage(on_solution(), Some(v.max))

      }
    case _ => (a) => master ! SolutionMessage(on_solution(), None)
  }

  val search: oscar.algo.search.Branching = objv match {
    case null => modelDeclaration.getSearch(cpmodel)
    case _ => new IntBoundaryUpdateSearchWrapper(modelDeclaration.getSearch(cpmodel), this, cpmodel.cpObjective)
  }
  //val search: oscar.algo.search.Branching = getSearch(cpmodel)

  /**
    * Process messages from master
    */
  def receive = {
    case m: HelloMessage => sender() ! m
    case StartMessage() => master ! AwaitingSPMessage()
    case DoSubproblemMessage(spid: Int, sp: Map[Int, Int]) =>
      log.info("received subproblem")
      Future {
        solve_subproblem(spid, sp)
      }
    case BoundUpdateMessage(newBound: Int) =>
      this.update_boundary(newBound)
    case AllDoneMessage() =>
      context.stop(self)
    case _ => log.info("received unknown message")
  }

  /**
    * Solve the current subproblem; should be called from a Future.
    */
  def solve_subproblem(spid: Int, sp: Map[Int, Int]): Unit = {
    val t0 = getThreadCpuTime
    val info = modelDeclaration.apply(cpmodel) {
      cpmodel.cpSolver.startSubjectTo() {
        cpmodel.cpObjective.tightenMode = TightenType.NoTighten
        for ((variable, value) <- sp) {
          val v: CPIntVar = cpmodel.intRepresentatives.get(variable).realCPVar
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
    }
    cpmodel.cpSolver.searchEngine.clearOnSolution()
    val t1 = getThreadCpuTime
    master ! DoneMessage(spid, t1 - t0, new SearchStatistics(info))
  }

  def get_boundary(): Int = boundary

  def update_boundary(newval: Int) = boundary = newval
}

object SolverActor {
  def props[RetVal](modelDeclaration: ModelDeclaration with DecomposedCPSolve[RetVal], master: ActorRef): Props =
    Props(classOf[SolverActor[RetVal]], modelDeclaration, master)
}

