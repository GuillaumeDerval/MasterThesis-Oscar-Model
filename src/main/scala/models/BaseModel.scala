package models

import misc.ModelVarStorage
import vars.IntVar
import vars.domainstorage.int.IntDomainStorage

/**
  * Created by dervalguillaume on 12/04/16.
  */
object BaseModel {
  def apply(declaration: ModelDeclaration) = UninstantiatedModel(declaration, Nil, ModelVarStorage[IntVar, IntDomainStorage](), NoOptimisation())
}
