package solvers.cp.decompositions

import misc.CartesianProduct
import models.{MemoCPModel, UninstantiatedModel}
import solvers.cp.{SubProblem, SubProblemCartesianProductLog}
import vars.IntVar

abstract class DecompositionInfoAdd(baseDecomposition: DecompositionStrategy) extends DecompositionStrategy {
  /**
    * Decompose the problem
    *
    * @param baseModel the model to decompose
    * @param count the (minimum) number of subproblems wanted
    * @return A list of subproblems
    */
  override def decompose(baseModel: UninstantiatedModel, count: Int): List[SubProblem] = {
    val model = new MemoCPModel(baseModel.removeOptimisation())
    model.apply {
      baseDecomposition.decompose(baseModel, count).map((sp) => {
        model.pushState()
        for(c <- sp.constraints)
          model.post(c)
        val out = addInfo(sp, model)
        model.popState()
        out
      })
    }
  }

  /**
    * Add info to a problem
    *
    * @param subProblem
    * @return the same subProblem, but with additionnal info
    */
  def addInfo(subProblem: SubProblem, model: MemoCPModel): SubProblem
}

class DecompositionAddCartProdInfo(baseDecomposition: DecompositionStrategy, allVars: Iterable[IntVar]) extends DecompositionInfoAdd(baseDecomposition) {
  /**
    * Add info to a problem
    *
    * @param subProblem
    * @return the same subProblem, but with additionnal info
    */
  override def addInfo(subProblem: SubProblem, memoCPModel: MemoCPModel): SubProblem = {
    subProblem.addData(SubProblemCartesianProductLog, CartesianProduct.computeLog(allVars))
  }
}