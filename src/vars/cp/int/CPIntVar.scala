package vars.cp.int

import vars.IntVarImplem
import vars.cp.CPVar
import vars.domainstorage.int.IntDomainStorage

trait CPIntVar extends CPVar with IntVarImplem with IntDomainStorage {}