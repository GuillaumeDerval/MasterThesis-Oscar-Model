package vars.cp.int

import vars.domainstorage.int.AdaptableIntDomainStorage

class CPAdaptableIntVar(content: CPIntVar) extends AdaptableIntDomainStorage(content) with CPIntVar {
  def print = content.print
}