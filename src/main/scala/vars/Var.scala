package vars

import models.{ModelDeclaration, UninstantiatedModel}
import vars.domainstorage.DomainStorage

/**
 * The top class for any model variable
 *
 * @param model_decl: the ModelDeclaration associated with this Var
 */
abstract class Var(val model_decl: ModelDeclaration, val varid: Int)