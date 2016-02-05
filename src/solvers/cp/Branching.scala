package solvers.cp

import models.instantiated.InstantiatedCPModel
import vars.IntVar

/**
  * Created by dervalguillaume on 4/11/15.
  */
trait Branching {
  def forModel(model: InstantiatedCPModel): oscar.algo.search.Branching
  def apply(model: InstantiatedCPModel) = forModel(model)
}

object Branching{
  def apply(a: => Seq[oscar.algo.search.Alternative]) = new CustomBranching(a)
  val noAlternative = oscar.algo.search.noAlternative
  type Alternative = oscar.algo.search.Alternative
  def branch(left : => Unit)(right : => Unit) : Seq[Alternative] = oscar.algo.search.branch(left)(right)
  def branchOne(action : => Unit) : Seq[Alternative] = oscar.algo.search.branchOne(action)
  def branchAll[A](indexes : Seq[A])(f : A => Unit) : Seq[Alternative] = oscar.algo.search.branchAll(indexes)(f)

  def binaryIdx(variables: Array[IntVar], varHeuristic: (Int => Int), valHeuristic: (Int => Int)): Branching = {
    new BinaryBranching(variables, varHeuristic, valHeuristic)
  }

  def binaryIdx(variables: Array[IntVar], varHeuristic: (Int => Int)): Branching = {
    binaryIdx(variables, varHeuristic, variables(_).min)
  }

  def binary(variables: Array[IntVar]): Branching = {
    binaryIdx(variables, variables(_).min, variables(_).min)
  }

  def binary(variables: Traversable[IntVar], varHeuris: (IntVar => Int), valHeuris: (IntVar => Int)): Branching = {
    val vars = variables.toArray
    binaryIdx(vars, (i: Int) => varHeuris(vars(i)), (i: Int) => valHeuris(vars(i)))
  }

  def binaryFirstFailIdx(variables: Seq[IntVar], valHeuris: (Int => Int)): Branching = {
    val vars = variables.toArray
    binaryIdx(vars, vars(_).size, valHeuris)
  }

  def binaryFirstFail(variables: Seq[IntVar]): Branching = {
    val vars = variables.toArray
    binaryFirstFailIdx(vars, vars(_).min)
  }

  def binaryFirstFail(variables: Seq[IntVar], valHeuris: (IntVar => Int)): Branching = {
    val vars = variables.toArray
    binaryFirstFailIdx(vars, i => valHeuris(vars(i)))
  }

  def binaryLastConflict(variables: Array[IntVar]): Branching = {
    binaryLastConflict(variables, variables(_).size, variables(_).min)
  }

  def binaryLastConflict(variables: Array[IntVar], varHeuristic: (Int => Int)): Branching = {
    binaryLastConflict(variables, varHeuristic, variables(_).min)
  }

  def binaryLastConflict(variables: Array[IntVar], varHeuristic: (Int => Int), valHeuristic: (Int => Int)): Branching = {
    new BinaryLastConflict(variables, varHeuristic, valHeuristic)
  }
}