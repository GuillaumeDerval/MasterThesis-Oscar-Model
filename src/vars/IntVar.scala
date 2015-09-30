package vars

import misc.EmptyDomainException
import models.ModelDeclaration
import vars.domainstorage.int.IntDomainStorage

import scala.collection.mutable
import scala.util.Random

/**
 * Represents a variable with Integer domain
 * @param model_decl: the ModelDeclaration associated with this Var
 */
class IntVar(model_decl: ModelDeclaration, storage: IntDomainStorage) extends Var(model_decl, storage) with IntVarLike {
  def this(model_decl: ModelDeclaration, min: Int, max: Int) = this(model_decl, IntDomainStorage(min, max))

  def this(model_decl: ModelDeclaration, content: Set[Int]) = this(model_decl, IntDomainStorage(content))

  def this(model_decl: ModelDeclaration, content: mutable.SortedSet[Int]) = this(model_decl, IntDomainStorage(content))

  def this(model_decl: ModelDeclaration, value: Int) = this(model_decl, IntDomainStorage(value))

  def getImplementation: IntVarImplem = model_decl.getCurrentModel.get_implementation(this).asInstanceOf[IntVarImplem]

  override def isBound: Boolean = getImplementation.isBound
  override def randomValue(implicit rand: Random): Int = getImplementation.randomValue(rand)
  override def updateMin(value: Int): Unit = getImplementation.updateMin(value)
  override def max: Int = getImplementation.max
  override def valueBefore(value: Int): Option[Int] = getImplementation.valueBefore(value)
  override def min: Int = getImplementation.min
  override def assign(value: Int): Unit = getImplementation.assign(value)
  override def valueAfter(value: Int): Option[Int] = getImplementation.valueAfter(value)
  override def iterator: Iterator[Int] = getImplementation.iterator
  override def hasValue(value: Int): Boolean = getImplementation.hasValue(value)
  override def removeValue(value: Int): Unit = getImplementation.removeValue(value)
  override def updateMax(value: Int): Unit = getImplementation.updateMax(value)
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