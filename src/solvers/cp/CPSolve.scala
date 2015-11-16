package solvers.cp

import models.Model

trait CPSolve {
  private var branching: Branching = null
  private var on_solution: Model => Unit = (_) => {}

  def getSearch = branching
  def setSearch(b: Branching): Unit = branching = b
  def setSearch(b: => Seq[oscar.algo.search.Alternative]): Unit = branching = Branching(b)
  def onSolution = on_solution
  def onSolution(s: => Unit): Unit = onSolution((a) => s)
  def onSolution(o: Model => Unit): Unit = on_solution = o
}