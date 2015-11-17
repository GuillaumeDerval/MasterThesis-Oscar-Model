package misc

import vars.IntVar

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

object SubsetProduct {
  /**
    * Find a subset of values such that the product of the element of the subset is the smallest possible greater than obj
    * If such a set could not be found, it implies that the product of all the elements are < obj: all the indexes are returned
    * @param obj the objective, a strictly positive integer
    * @param values the possible values, all strictly positive integers
    * @return an array of indices in the corresponding subset
    */
  def apply(obj: Int, values: Array[Int]): Array[Int] = {
    //d(i)(j) == does it exist a subset in the subset values(0..i) with cost j?
    val maxJ = obj*values.min
    val d = Array.tabulate(values.length, maxJ)((i, j) => false)
    d(0)(values(0)) = true

    var posI = 0
    var posJ = 0

    breakable {
      for (i <- 1 until values.length) {
        d(i)(values(i)) = true
        for (j <- 1 until maxJ) {
          d(i)(j) = d(i - 1)(j)
          if (!d(i)(j) && j % values(i) == 0)
            d(i)(j) = d(i - 1)(j / values(i))
          if(j >= obj && d(i)(j)){
            posI = i
            posJ = j
            break
          }
        }
      }
    }
    val b = new ArrayBuffer[Int]


    while(posI >= 0) {
      if(values(posI) == posJ){
        b += posI
        posI = -1
      }
      else if(posI != 0 && d(posI-1)(posJ)) {
        posI -= 1
      }
      else {
        b += posI
        posJ /= values(posI)
        posI -= 1
      }
    }

    b.toArray
  }
}
