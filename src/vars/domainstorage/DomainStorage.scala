package vars.domainstorage

import vars.VarImplem

/**
 * A basic type for domains stored inside models
 */
trait DomainStorage extends VarImplem
{
  /**
   * Returns a copy of the same type as the current one
   * @return
   */
  def copy(): DomainStorage
}
