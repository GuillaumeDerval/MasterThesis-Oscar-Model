package solvers.cp.branchings

import models.CPModel
import solvers.cp.branchings.Branching.Alternative
import vars.IntVar

class BinaryStaticOrderBranching(cp: CPModel, variables: Array[IntVar], valHeuris: (Int => Int)) extends Branching {
  def this(cp: CPModel, vars: Array[IntVar]) = this(cp, vars, vars(_).min)

  private[this] val nVariables = variables.length
  private[this] val depthRev = cp.getReversibleInt(0)
  private[this] var depth = 0

  final override def alternatives(): Seq[Alternative] = {
    // Cache
    depth = depthRev.value

    // Update depth
    while (depth < nVariables && variables(depth).isBound) depth += 1

    if (depth == nVariables) Branching.noAlternative
    else {
      // Trail new depth
      depthRev.value = depth
      // Alternatives
      val variable = variables(depth)
      val value = valHeuris(depth)
      Branching.branch(cp.post(variable == value))(cp.post(variable != value))
    }
  }
}