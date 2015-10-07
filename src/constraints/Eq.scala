package constraints
import vars.IntVar

/**
 * An equality constraint
 * @param x: first variable id
 * @param y: second variable id
 */
class Eq(val x: Int, val y: Int) extends Constraint {
  /**
   * An equality constraint (xv == yv)
   * @param xv: first variable
   * @param yv: second variable
   */
  def this(xv: IntVar, yv: IntVar) = this(xv.viewid, yv.viewid)
}