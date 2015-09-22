package vars.cp

import vars.VarImplem
import vars.domainstorage.DomainStorage

/**
 * An instantiated CP variable
 */
trait CPVar extends VarImplem {
  def print: String
}