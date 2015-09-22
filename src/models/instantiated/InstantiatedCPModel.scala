package models.instantiated

import misc.UnionFindStorage
import models.uninstantiated.UninstantiatedModel
import models.{Model, ModelDeclaration}
import vars.cp.CPVar
import vars.domainstorage.DomainStorage

/**
 * An instantiated model, containing CPVars as implementations
 * @param p: the parent Model
 */
class InstantiatedCPModel(p: UninstantiatedModel) extends InstantiatedModel
{
  override type Implementation = CPVar
  override val parent: Option[Model] = Some(p)
  override val declaration: ModelDeclaration = p.declaration
  override val domains: UnionFindStorage[CPVar] = UnionFindStorage[CPVar, DomainStorage](p.domains, convert_domainstorage_to_cpvar)

  def convert_domainstorage_to_cpvar(v: DomainStorage): CPVar = {
    ???
  }
}