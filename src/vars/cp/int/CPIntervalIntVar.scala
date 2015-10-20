package vars.cp.int

import vars.domainstorage.int.IntervalDomainStorage

class CPIntervalIntVar(min_value: Int, max_value: Int, repr_name: Option[String] = None) extends IntervalDomainStorage(min_value, max_value, repr_name) with CPIntVar {
  def print = "" + min_value + " " + max_value
}