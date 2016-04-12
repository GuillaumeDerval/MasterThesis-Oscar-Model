package solvers.cp.branchings

import models.CPModel
import oscar.algo.search

/**
  * Created by dervalguillaume on 4/11/15.
  */
class CustomBranching(alternatives: => Seq[search.Alternative]) extends Branching{
  override def forModel(model: CPModel): search.Branching = search.Branching(alternatives)
}
