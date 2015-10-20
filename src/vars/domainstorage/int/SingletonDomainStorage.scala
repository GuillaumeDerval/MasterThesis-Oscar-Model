package vars.domainstorage.int

import misc.EmptyDomainException

import scala.util.Random

/**
 * A domain that stores a single integer
 * @param single_value
 */
class SingletonDomainStorage(single_value: Int, private val repr_name: Option[String] = None) extends IntDomainStorage {
  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  override def isBound: Boolean = true

  /**
   * @return A random value in the domain of the variable (uniform distribution)
   */
  override def randomValue(implicit rand: Random): Int = single_value

  /**
   * Remove from the domain all values < val. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException: if the domain becomes empty
   */
  override def updateMin(value: Int): Unit = if (min > single_value) throw new EmptyDomainException()

  /**
   * @return  the minimum value in the domain
   */
  override def min: Int = single_value

  /**
   * @param value
   * @return the largest value < val in the domain, None if there is not value < val in the domain
   */
  override def valueBefore(value: Int): Option[Int] = None

  /**
   * Reduce the domain to the singleton {val}. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException
   */
  override def assign(value: Int): Unit = if (value != single_value) throw new EmptyDomainException()

  /**
   * @param value
   * @return the smallest value > val in the domain, None if there is not value > val in the domain
   */
  override def valueAfter(value: Int): Option[Int] = None

  override def iterator: Iterator[Int] = List(single_value).iterator

  /**
   * Test if a value is in the domain
   * @param value: value to test
   * @return  true if the domain contains the value val, false otherwise
   */
  override def hasValue(value: Int): Boolean = value == single_value

  /**
   * Remove val from the domain. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException: if the domain becomes empty
   */
  override def removeValue(value: Int): Unit = if (value == single_value) throw new EmptyDomainException()

  /**
   * Remove from the domain all values > val. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException: if the domain becomes empty
   */
  override def updateMax(value: Int): Unit = if (max < single_value) throw new EmptyDomainException()

  /**
   * @return  the maximum value in the domain
   */
  override def max: Int = single_value

  /**
   * Returns a copy of the same type as the current one
   * @return
   */
  override def copy(): SingletonDomainStorage = new SingletonDomainStorage(single_value, getRepresentativeName)

  /**
   * Return a representative name for this var(-like), if one was given
   */
  override def getRepresentativeName: Option[String] = repr_name
}
