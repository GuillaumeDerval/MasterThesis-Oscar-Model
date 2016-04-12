package solvers.cp.branchings

import models.CPModel
import oscar.algo.search
import oscar.cp._
import oscar.cp.searches.Decision
import vars.IntVar


class NaryStaticBranching(array: Array[IntVar]) extends Branching {
  override def forModel(model: CPModel): search.Branching = {
    new NaryStaticBranchingOscar(array.map(a => model.getRepresentative(a).realCPVar))
  }
}

class NaryStaticBranchingOscar(variables: Array[CPIntVar]) extends oscar.algo.search.Branching {
  private[this] val store = variables(0).store
  private[this] val nVariables = variables.length

  final override def alternatives(): Seq[Alternative] = {
    var variable: CPIntVar = null
    for(v <- variables) {
      if(null == variable && !v.isBound) {
        variable = v
      }
    }
    if (null == variable) noAlternative
    else
      variable.toArray.sortWith(_ < _).map(i => Decision.assign(variable, i))
  }
}