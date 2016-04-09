package solvers.cp.branchings

import models.instantiated.InstantiatedCPModel
import oscar.algo.search

/**
  * Created by dervalguillaume on 4/11/15.
  */
class CustomBranching(alternatives: => Seq[search.Alternative]) extends Branching{
  override def forModel(model: InstantiatedCPModel): search.Branching = search.Branching(alternatives)
}
