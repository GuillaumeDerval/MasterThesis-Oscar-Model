package solvers.cp

import java.util
import java.util.concurrent.LinkedBlockingQueue

import misc.ComputeTimeTaken.computeTimeTaken
import misc.SearchStatistics
import misc.TimeHelper._
import models._
import models.instantiated.InstantiatedCPModel
import models.uninstantiated.UninstantiatedModel
import oscar.cp.TightenType
import solvers.cp.decompositions.{ClosureDecompositionStrategy, DecompositionStrategy, DecompositionStrategyToClosureConverter}
import vars.IntVar

class LocalParallelCPProgram[RetVal](md: ModelDeclaration with LocalDecomposedCPSolve[RetVal] = new ModelDeclaration() with LocalDecomposedCPSolve[RetVal])
  extends ModelProxy[LocalDecomposedCPSolve[RetVal], RetVal](md)
{
  implicit val program = this
  var threadsToLaunch = Runtime.getRuntime.availableProcessors()
  var subproblemsCount = 100*threadsToLaunch

  def setDecompositionStrategy(d: ClosureDecompositionStrategy): Unit = md.setDecompositionStrategy(d)
  def setDecompositionStrategy(d: DecompositionStrategy): Unit = md.setDecompositionStrategy(new DecompositionStrategyToClosureConverter(d))
  def getDecompositionStrategy: ClosureDecompositionStrategy = md.getDecompositionStrategy

  def solve(): (SearchStatistics, List[RetVal]) = solve(modelDeclaration.getCurrentModel)

  def solve(model: Model): (SearchStatistics, List[RetVal]) = {
    model match {
      case m: UninstantiatedModel => solve(m)
      case _ => sys.error("The model is already instantiated")
    }
  }

  def solve(model: UninstantiatedModel): (SearchStatistics, List[RetVal]) = {

    val subproblems = computeTimeTaken("Decomposition"){/*Random.shuffle(*/getDecompositionStrategy.decompose(model, subproblemsCount)/*)*/}

    println("Subproblems: "+subproblems.length.toString)

    val queue = new LinkedBlockingQueue[((InstantiatedCPModel) => Unit, Int)]()
    val outputQueue = new LinkedBlockingQueue[SolvingMessage]()

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

    val statWatcher = new StatisticsWatcher[RetVal]
    val watchers = Array[Watcher[RetVal]](statWatcher, pb)
    val watcher_thread = new Thread(new WatcherRunnable(watchers, outputQueue))

    watcher_thread.start()
    threads.foreach(_.start())
    pb.start()
    threads.foreach(_.join())
    outputQueue.add(AllDoneMessage())
    watcher_thread.join()
    statWatcher.get
  }

  class Subsolver(uninstantiatedModel: UninstantiatedModel,
                  subproblemQueue: util.Queue[((InstantiatedCPModel) => Unit, Int)],
                  outputQueue: util.Queue[SolvingMessage],
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

        val solution: Model => Unit = boundaryManager match {
          case Some(bm) => (a) => {
            val v = cpmodel.getRepresentative(objv)
            bm.update_boundary(v.max)
            outputQueue.add(SolutionMessage(modelDeclaration.onSolution(), Some(v.max)))
          }
          case _ => (a) => outputQueue.add(SolutionMessage(modelDeclaration.onSolution(), None))
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
            outputQueue.add(DoneMessage(spid, t1-t0, new SearchStatistics(info)))
          }
        }
      }
    }
  }
}