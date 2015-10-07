package vars

import misc.EmptyDomainException
import models.ModelDeclaration

import scala.util.Random

/**
 * Represent a view for an integer
 * @param modelDeclaration the ModelDeclaration linked with this view
 * @param implem_id the id of the implementation(domain) that is the base of this view
 * @param multiplier the multiplier applied to the initial domain. Cannot be 0.
 * @param offset the offset applied (after the multiplier) to the initial domain
 */
class IntView(val modelDeclaration: ModelDeclaration, implem_id: Int, val multiplier: Int, val offset: Int) extends IntVarLike
{
  assert(multiplier != 0, "Multiplier cannot be 0!")

  /**
   * Copy the IntView into a new IntView
   */
  def copy(): IntView = new IntView(modelDeclaration, implem_id, multiplier, offset)

  /**
   * Get the domain(implementation) to which this view refers
   */
  protected def getImplementation: IntVarImplem = modelDeclaration.getCurrentModel.getIntVarImplementation(implem_id)

  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  override def isBound: Boolean = getImplementation.isBound

  /**
   * @return A random value in the domain of the variable (uniform distribution)
   */
  override def randomValue(implicit rand: Random): Int = getImplementation.randomValue(rand) * multiplier + offset

  /**
   * Remove from the domain all values < val. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException: if the domain becomes empty
   */
  override def updateMin(value: Int): Unit = {
    val tmp = value-offset
    if(multiplier > 0) {
      if(tmp % multiplier != 0) //ceil
        getImplementation.updateMin(tmp/multiplier+1)
      else
        getImplementation.updateMin(tmp/multiplier)
    }
    else
      getImplementation.updateMax(tmp/multiplier) //auto floor
  }

  /**
   * @return  the maximum value in the domain
   */
  override def max: Int = {
    if(multiplier > 0)
      getImplementation.max*multiplier+offset
    else
      getImplementation.min*multiplier+offset
  }

  /**
   * @param value
   * @return the largest value < val in the domain, None if there is not value < val in the domain
   */
  override def valueBefore(value: Int): Option[Int] = ???

  /**
   * @return  the minimum value in the domain
   */
  override def min: Int = {
    if(multiplier > 0)
      getImplementation.min*multiplier+offset
    else
      getImplementation.max*multiplier+offset
  }

  /**
   * Reduce the domain to the singleton {val}. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException
   */
  override def assign(value: Int): Unit = {
    val tmp = value - offset
    if(tmp % multiplier == 0)
      getImplementation.assign(tmp/multiplier)
    else
      throw new EmptyDomainException()
  }

  /**
   * @param value
   * @return the smallest value > val in the domain, None if there is not value > val in the domain
   */
  override def valueAfter(value: Int): Option[Int] = ???

  override def iterator: Iterator[Int] = ???

  /**
   * Test if a value is in the domain
   * @param value: value to test
   * @return  true if the domain contains the value val, false otherwise
   */
  override def hasValue(value: Int): Boolean = {
    val tmp = value - offset
    if(tmp % multiplier == 0)
      getImplementation.hasValue(tmp/multiplier)
    else
      false
  }

  /**
   * Remove val from the domain. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException: if the domain becomes empty
   */
  override def removeValue(value: Int): Unit = {
    val tmp = value - offset
    if(tmp % multiplier == 0)
      getImplementation.removeValue(tmp/multiplier)
  }

  /**
   * Remove from the domain all values > val. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException: if the domain becomes empty
   */
  override def updateMax(value: Int): Unit = {
    val tmp = value-offset
    if(multiplier < 0) {
      if(tmp % multiplier != 0) //ceil
        getImplementation.updateMin(tmp/multiplier+1)
      else
        getImplementation.updateMin(tmp/multiplier)
    }
    else
      getImplementation.updateMax(tmp/multiplier) //auto floor
  }
}
