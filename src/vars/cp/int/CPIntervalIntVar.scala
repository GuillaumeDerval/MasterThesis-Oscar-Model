package vars.cp.int

import vars.domainstorage.int.IntervalDomainStorage

class CPIntervalIntVar(min_value: Int, max_value: Int) extends IntervalDomainStorage(min_value, max_value) with CPIntVar {
  def print = "" + min_value + " " + max_value
}