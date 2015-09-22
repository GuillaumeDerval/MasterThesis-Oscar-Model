package vars.domainstorage.int

import misc.{CannotBecomeSparseException, EmptyDomainException}

import scala.util.Random

/**
 * A proxy to other IntDomainStorage. Allow to change representation when needed.
 * Note: this class supports IntervalDomainStorage, SetDomainStorage, and SingletonDomainStorage
 * @param content: another domain to proxy
 */
class AdaptableIntDomainStorage(private var content: IntDomainStorage) extends IntDomainStorage {
  //Check type
  if (!content.isInstanceOf[IntervalDomainStorage] && !content.isInstanceOf[SetDomainStorage] &&
    !content.isInstanceOf[SingletonDomainStorage])
    throw new IllegalArgumentException("Invalid type")

  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  override def isBound: Boolean = content.isBound

  /**
   * @return A random value in the domain of the variable (uniform distribution)
   */
  override def randomValue(implicit rand: Random): Int = content.randomValue(rand)

  /**
   * @return  the maximum value in the domain
   */
  override def max: Int = content.max

  /**
   * @param value
   * @return the largest value < val in the domain, None if there is not value < val in the domain
   */
  override def valueBefore(value: Int): Option[Int] = content.valueBefore(value)

  /**
   * @return  the minimum value in the domain
   */
  override def min: Int = content.min

  /**
   * @param value
   * @return the smallest value > val in the domain, None if there is not value > val in the domain
   */
  override def valueAfter(value: Int): Option[Int] = content.valueAfter(value)

  override def iterator: Iterator[Int] = content.iterator

  /**
   * Test if a value is in the domain
   * @param value: value to test
   * @return  true if the domain contains the value val, false otherwise
   */
  override def hasValue(value: Int): Boolean = content.hasValue(value)

  /**
   * Reduce the domain to the singleton {val}. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException: if the initial do not contain val
   */
  override def assign(value: Int): Unit = {
    if (content.hasValue(value)) content = new SingletonDomainStorage(value)
    else throw new EmptyDomainException()
  }

  /**
   * Remove from the domain all values < val. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException: if the domain becomes empty
   */
  override def updateMin(value: Int): Unit = {
    content.updateMin(value)
    simplify()
  }

  /**
   * Remove from the domain all values > val. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException: if the domain becomes empty
   */
  override def updateMax(value: Int): Unit = {
    content.updateMax(value)
    simplify()
  }

  /**
   * Remove val from the domain. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException: if the domain becomes empty
   */
  override def removeValue(value: Int): Unit = {
    try {
      content.removeValue(value)
    }
    catch {
      case e: CannotBecomeSparseException =>
        content = new SetDomainStorage(content.toSet)
        content.removeValue(value)
    }
    simplify()
  }

  /**
   * Tries to simplify the way the data is stored
   */
  private def simplify() = {
    if (content.isInstanceOf[SetDomainStorage]) {
      if (content.size == 1)
        content = new SingletonDomainStorage(content.min)
      else if (content.isContinuous)
        content = new IntervalDomainStorage(content.min, content.max)
    }
  }

  /**
   * Returns a copy of the same type as the current one
   */
  override def copy(): AdaptableIntDomainStorage = new AdaptableIntDomainStorage(content.copy())
}
