package solvers.cp.decompositions

import models.uninstantiated.UninstantiatedModel
import vars.IntVar

/**
  * Created by dervalguillaume on 6/11/15.
  */
trait DecompositionStrategy {
  def decompose(model: UninstantiatedModel, count: Integer): List[Map[IntVar, Int]]
}
