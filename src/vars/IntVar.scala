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
class IntVar(val viewid: Int, model_decl: ModelDeclaration) extends Var(model_decl) with IntVarLike {
  def getView: IntView = model_decl.getCurrentModel.getIntView(viewid)
  override def isBound: Boolean = getView.isBound
  override def randomValue(implicit rand: Random): Int = getView.randomValue(rand)
  override def updateMin(value: Int): Unit = getView.updateMin(value)
  override def max: Int = getView.max
  override def valueBefore(value: Int): Option[Int] = getView.valueBefore(value)
  override def min: Int = getView.min
  override def assign(value: Int): Unit = getView.assign(value)
  override def valueAfter(value: Int): Option[Int] = getView.valueAfter(value)
  override def iterator: Iterator[Int] = getView.iterator
  override def hasValue(value: Int): Boolean = getView.hasValue(value)
  override def removeValue(value: Int): Unit = getView.removeValue(value)
  override def updateMax(value: Int): Unit = getView.updateMax(value)
}

object IntVar {
  def apply(minValue: Int, maxValue: Int)(implicit model_decl: ModelDeclaration) = {
    init_intvar(IntDomainStorage(minValue, maxValue), model_decl)
  }

  def apply(content: Set[Int])(implicit model_decl: ModelDeclaration) = {
    init_intvar(IntDomainStorage(content), model_decl)
  }

  def apply(content: mutable.SortedSet[Int])(implicit model_decl: ModelDeclaration) = {
    init_intvar(IntDomainStorage(content), model_decl)
  }

  def apply(value: Int)(implicit model_decl: ModelDeclaration) = {
    init_intvar(IntDomainStorage(value), model_decl)
  }

  private def init_intvar(storage: IntDomainStorage, model_decl: ModelDeclaration,
                          multiplier: Int = 1, offset: Int = 0): IntVar = {
    val implem_id = model_decl.getCurrentModel.asInstanceOf[UninstantiatedModel].addNewIntVarImplementation(storage)
    val view = new IntView(model_decl, implem_id, multiplier, offset)
    val view_id = model_decl.getCurrentModel.asInstanceOf[UninstantiatedModel].addNewIntView(view)
    new IntVar(view_id, model_decl)
  }
}