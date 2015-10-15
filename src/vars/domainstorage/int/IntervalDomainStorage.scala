package vars.domainstorage.int

import misc.{CannotBecomeSparseException, EmptyDomainException}

import scala.util.Random

/**
 * Stores a continuous interval
 * @param min_value: the minimum value (inclusive)
 * @param max_value: the maximum value (inclusive)
 */
class IntervalDomainStorage(private var min_value: Int, private var max_value: Int) extends IntDomainStorage {
  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  override def isBound: Boolean = min_value == max_value

  /**
   * @return A random value in the domain of the variable (uniform distribution)
   */
  override def randomValue(implicit rand: Random): Int = min_value + rand.nextInt(size)

  /**
   * @return the size of the domain
   */
  override def size: Int = min_value - max_value + 1

  /**
   * @param value
   * @return the largest value < val in the domain, None if there is not value < val in the domain
   */
  override def valueBefore(value: Int): Option[Int] = if (value - 1 >= min_value) Option[Int](value - 1) else None

  /**
   * @param value
   * @return the smallest value > val in the domain, None if there is not value > val in the domain
   */
  override def valueAfter(value: Int): Option[Int] = if (value + 1 <= max_value) Option[Int](value + 1) else None

  override def iterator: Iterator[Int] = Range(min_value, max_value+1).iterator

  /**
   * Test if a value is in the domain
   * @param value: value to test
   * @return  true if the domain contains the value val, false otherwise
   */
  override def hasValue(value: Int): Boolean = value >= min_value && value <= max_value

  /**
   * Reduce the domain to the singleton {val}. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException
   */
  override def assign(value: Int): Unit = {
    if (min_value <= value && value <= max_value) {
      min_value = value
      max_value = value
    }
    else
      throw new EmptyDomainException()
  }

  /**
   * Remove from the domain all values < val. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException: if the domain becomes empty
   */
  override def updateMin(value: Int): Unit = {
    if (value > max_value)
      throw new EmptyDomainException()
    min_value = value
  }

  /**
   * Remove from the domain all values > val. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException: if the domain becomes empty
   */
  override def updateMax(value: Int): Unit = {
    if (value < min_value)
      throw new EmptyDomainException()
    max_value = value
  }

  /**
   * Remove val from the domain. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException: if the domain becomes empty
   * @throws CannotBecomeSparseException: as this implementation cannot become sparse,
   *                                    it will launch an exception when you attempt to do that.
   */
  override def removeValue(value: Int): Unit = {
    if (min_value == max_value && min_value == value)
      throw new EmptyDomainException()
    if (min_value == value)
      min_value = value + 1
    else if (max == value)
      max_value = value - 1
    else
      throw new CannotBecomeSparseException
  }

  /**
   * @return  the maximum value in the domain
   */
  override def max: Int = max_value

  /**
   * @return  the minimum value in the domain
   */
  override def min: Int = min_value

  /**
   * Returns a copy of the same type as the current one
   * @return
   */
  override def copy(): IntervalDomainStorage = new IntervalDomainStorage(min_value, max_value)
}
