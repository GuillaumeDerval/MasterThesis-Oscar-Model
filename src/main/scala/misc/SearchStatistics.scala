package misc

class SearchStatistics(val nNodes: Int, val nFails: Int, val time: Long, val completed: Boolean,
                       val timeInTrail: Long, val maxTrailSize: Int, val nSols: Int) extends Serializable {

  /**
    * Copy constructor
    */
  def this(a: oscar.algo.search.SearchStatistics) = {
    this(a.nNodes, a.nFails, a.time, a.completed, a.timeInTrail, a.maxTrailSize, a.nSols)
  }

  override val toString: String = s"nNodes: $nNodes\nnFails: $nFails\ntime(ms): $time\ncompleted: $completed\ntimeInTrail: $timeInTrail\nnSols: $nSols\n"
}