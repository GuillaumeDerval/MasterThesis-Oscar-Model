package models

/**
 * Trait used by classes that provides a search for a solvers.CPSolver
 */
trait CPSearch extends SolutionManager {
  private var search: Model => Unit = null

  def getSearch = search

  def setSearch(s: => Unit): Unit = setSearch((_) => s)

  def setSearch(s: Model => Unit): Unit = search = s
}