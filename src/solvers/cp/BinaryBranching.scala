package solvers.cp

import models.instantiated.InstantiatedCPModel
import oscar.algo.search
import vars.IntVar

class BinaryBranching(array: Array[IntVar], varHeuristic: (Int => Int), valHeuristic: (Int => Int)) extends Branching {
  override def forModel(model: InstantiatedCPModel): search.Branching = {
    new oscar.cp.searches.BinaryBranching(array.map(a => model.getRepresentative(a).realCPVar), varHeuristic, valHeuristic)
  }
}

class BinaryLastConflict(array: Array[IntVar], varHeuristic: Int => Int, valHeuristic: Int => Int) extends Branching {
  override def forModel(model: InstantiatedCPModel): search.Branching = {
    new oscar.cp.searches.BinaryLastConflict(array.map(a => model.getRepresentative(a).realCPVar), varHeuristic, valHeuristic)
  }
}

class BinaryStaticOrderBranching(vars: Array[IntVar], valHeuris: Int=> Int) extends Branching {
  override def forModel(model: InstantiatedCPModel): search.Branching = {
    new oscar.cp.searches.BinaryStaticOrderBranching(vars.map(a => model.getRepresentative(a).realCPVar), valHeuris)
  }
}