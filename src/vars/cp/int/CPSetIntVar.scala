package vars.cp.int

import vars.domainstorage.int.SetDomainStorage

import scala.collection.mutable

class CPSetIntVar(content: mutable.SortedSet[Int]) extends SetDomainStorage(content) with CPIntVar {
  def this(content: Set[Int]) = this(mutable.SortedSet(content.toList: _*))

  def print = content.toString()
}