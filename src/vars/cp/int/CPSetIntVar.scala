package vars.cp.int

import vars.domainstorage.int.SetDomainStorage

import scala.collection.mutable

class CPSetIntVar(content: mutable.SortedSet[Int], repr_name: Option[String] = None) extends SetDomainStorage(content, repr_name) with CPIntVar {
  def this(content: Set[Int], repr_name: Option[String]) = this(mutable.SortedSet(content.toList: _*), repr_name)
  def this(content: Set[Int]) = this(mutable.SortedSet(content.toList: _*))

  def print = content.toString()
}