package examples

import constraints.AllDifferent
import models.UninstantiatedModel
import models.operators.SimplifySum
import solvers.cp.branchings.Branching
import solvers.cp.decompositions.CartProdRefinement
import solvers.cp.{DistributedCPApp, DistributedCPAppConfig}
import vars.IntVar
import visualisation.ConstraintsVisualisation

/**
  * Created by dervalguillaume on 2/06/16.
  */
object SendMoreMoney extends DistributedCPApp[Unit] with App {
  // variables
  val S = IntVar(0,9, Some("S"))
  val E = IntVar(0,9, Some("E"))
  val N = IntVar(0,9, Some("N"))
  val D = IntVar(0,9, Some("D"))
  val M = IntVar(0,9, Some("M"))
  val O = IntVar(0,9, Some("O"))
  val R = IntVar(0,9, Some("R"))
  val Y = IntVar(0,9, Some("Y"))
  val all = Array(S,E,N,D,M,O,R,Y)

  // constraints
  add(       S*1000 + E*100 + N*10 + D +
    M*1000 + O*100 + R*10 + E ==
    M*10000 + O*1000 + N*100 + E*10 + Y)
  //add(AllDifferent(all))

  val a = new ConstraintsVisualisation(SimplifySum(this.getCurrentModel.asInstanceOf[UninstantiatedModel]).constraints.toArray, "")
  a.display()
}