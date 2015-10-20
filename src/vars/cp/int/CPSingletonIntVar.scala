package vars.cp.int

import vars.domainstorage.int.SingletonDomainStorage

class CPSingletonIntVar(value: Int, repr_name: Option[String] = None) extends SingletonDomainStorage(value, repr_name) with CPIntVar {
  def print = value.toString
}
