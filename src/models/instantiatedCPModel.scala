package models

import misc.UnionFindStorage
import vars.cp.CPVar
import vars.cp.int.CPIntVar
import vars.domainstorage.DomainStorage

/**
 * An instantiated model, containing CPVars as implementations
 * @param model: the parent Model
 */
class InstantiatedCPModel(val declaration: ModelDeclaration,
                          protected val domains: UnionFindStorage[CPVar],
                          val parent: Option[Model]) extends Model
{
  override type Implementation = CPVar
}