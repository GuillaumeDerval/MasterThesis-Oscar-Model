package misc

import misc.TimeHelper.getClockTime

/**
  * Created by dervalguillaume on 15/11/15.
  */
object ComputeTimeTaken {
  def computeTimeTaken[T](name: String)(v: => T): T = {
    val t0 = getClockTime
    val r: T = v
    val t1 = getClockTime
    println("Time taken by "+name+ ": "+(t1.toDouble - t0.toDouble)/math.pow(10, 9) +" s")
    r
  }
}
