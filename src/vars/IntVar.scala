package vars

import misc.EmptyDomainException
import models.ModelDeclaration
import models.uninstantiated.UninstantiatedModel
import vars.domainstorage.int.IntDomainStorage

import scala.collection.mutable
import scala.util.Random

/**
 * Represents a variable with Integer domain
 * @param model_decl: the ModelDeclaration associated with this Var
 */
class IntVar(model_decl: ModelDeclaration, storage: IntDomainStorage) extends Var(model_decl, storage) with IntVarLike {
  override val varid = model_decl.getCurrentModel.asInstanceOf[UninstantiatedModel].addNewRepresentative(storage)

  protected def getRepresentative: IntVarImplem = model_decl.getCurrentModel.getRepresentative(this).asInstanceOf[IntVarImplem]
  override def isBound: Boolean = getRepresentative.isBound
  override def randomValue(implicit rand: Random): Int = getRepresentative.randomValue(rand)
  override def updateMin(value: Int): Unit = getRepresentative.updateMin(value)
  override def max: Int = getRepresentative.max
  override def valueBefore(value: Int): Option[Int] = getRepresentative.valueBefore(value)
  override def min: Int = getRepresentative.min
  override def assign(value: Int): Unit = getRepresentative.assign(value)
  override def valueAfter(value: Int): Option[Int] = getRepresentative.valueAfter(value)
  override def iterator: Iterator[Int] = getRepresentative.iterator
  override def hasValue(value: Int): Boolean = getRepresentative.hasValue(value)
  override def removeValue(value: Int): Unit = getRepresentative.removeValue(value)
  override def updateMax(value: Int): Unit = getRepresentative.updateMax(value)
}

object IntVar {
  def apply(minValue: Int, maxValue: Int)(implicit model_decl: ModelDeclaration) = {
    new IntVar(model_decl, IntDomainStorage(minValue, maxValue))
  }

  def apply(content: Set[Int])(implicit model_decl: ModelDeclaration) = {
    new IntVar(model_decl, IntDomainStorage(content))
  }

  def apply(content: mutable.SortedSet[Int])(implicit model_decl: ModelDeclaration) = {
    new IntVar(model_decl, IntDomainStorage(content))
  }

  def apply(value: Int)(implicit model_decl: ModelDeclaration) = {
    new IntVar(model_decl, IntDomainStorage(value))
  }
}