package vars

import models.ModelDeclaration
import vars.domainstorage.int.IntDomainStorage

import scala.collection.mutable

/**
 * Represents a variable with Integer domain
 * @param model_decl: the ModelDeclaration associated with this Var
 */
class IntVar(model_decl: ModelDeclaration, storage: IntDomainStorage) extends Var(model_decl, storage) {
  def this(model_decl: ModelDeclaration, min: Int, max: Int) = this(model_decl, IntDomainStorage(min, max))

  def this(model_decl: ModelDeclaration, content: Set[Int]) = this(model_decl, IntDomainStorage(content))

  def this(model_decl: ModelDeclaration, content: mutable.SortedSet[Int]) = this(model_decl, IntDomainStorage(content))

  def this(model_decl: ModelDeclaration, value: Int) = this(model_decl, IntDomainStorage(value))

  def getImplementation: IntVarImplem = model_decl.getCurrentModel.get_implementation(this).asInstanceOf[IntVarImplem]
}

object IntVar {
  def apply(minValue: Int, maxValue: Int)(implicit model_decl: ModelDeclaration) = {
    new IntVar(model_decl, minValue, maxValue)
  }

  def apply(content: Set[Int])(implicit model_decl: ModelDeclaration) = {
    new IntVar(model_decl, content)
  }

  def apply(content: mutable.SortedSet[Int])(implicit model_decl: ModelDeclaration) = {
    new IntVar(model_decl, content)
  }

  def apply(value: Int)(implicit model_decl: ModelDeclaration) = {
    new IntVar(model_decl, value)
  }
}