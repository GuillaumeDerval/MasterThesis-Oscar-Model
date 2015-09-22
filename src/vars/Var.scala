package vars

import models.{UninstantiatedModel, ModelDeclaration}
import vars.domainstorage.DomainStorage

/**
 * The top class for any model variable
 * @param model_decl: the ModelDeclaration associated with this Var
 */
abstract class Var(val model_decl: ModelDeclaration, domainstorage: DomainStorage) {
  assert(model_decl.getCurrentModel.isInstanceOf[UninstantiatedModel], "New model variables can only be created in uninstantiated models")
  val varid = model_decl.getCurrentModel.asInstanceOf[UninstantiatedModel].add_new_var(domainstorage)
}