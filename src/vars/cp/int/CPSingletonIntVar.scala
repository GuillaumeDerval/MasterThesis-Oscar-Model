package vars.cp.int

import vars.domainstorage.int.SingletonDomainStorage

class CPSingletonIntVar(value: Int) extends SingletonDomainStorage(value) with CPIntVar {
  def print = value.toString
}
