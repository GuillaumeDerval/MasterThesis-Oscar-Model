package vars.domainstorage.int

import misc.EmptyDomainException

import scala.collection.mutable
import scala.util.Random

/**
 * Stores a set of int
 * @param content: the domain
 */
class SetDomainStorage(var content: mutable.SortedSet[Int]) extends IntDomainStorage {
  def this(content: Set[Int]) = this(mutable.SortedSet(content.toList: _*))

  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  override def isBound: Boolean = content.size == 1

  /**
   * @return A random value in the domain of the variable (uniform distribution)
   */
  override def randomValue(implicit rand: Random): Int = content.toVector(rand.nextInt(size))

  /**
   * @return the size of the domain
   */
  override def size: Int = content.size

  /**
   * @return  the maximum value in the domain
   */
  override def max: Int = content.max

  /**
   * @param value
   * @return the largest value < val in the domain, None if there is not value < val in the domain
   */
  override def valueBefore(value: Int): Option[Int] = content.to(value - 1).lastOption

  /**
   * @return  the minimum value in the domain
   */
  override def min: Int = content.min

  /**
   * @param value
   * @return the smallest value > val in the domain, None if there is not value > val in the domain
   */
  override def valueAfter(value: Int): Option[Int] = content.from(value + 1).headOption

  override def iterator: Iterator[Int] = content.iterator

  /**
   * Reduce the domain to the singleton {val}. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException
   */
  override def assign(value: Int): Unit = {
    if (hasValue(value)) {
      content.clear()
      content.add(value)
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
    val right = content.from(value)
    if (right.isEmpty)
      throw new EmptyDomainException()
    content = right
  }

  /**
   * Remove from the domain all values > val. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException: if the domain becomes empty
   */
  override def updateMax(value: Int): Unit = {
    val left = content.to(value)
    if (left.isEmpty)
      throw new EmptyDomainException()
    content = left
  }

  /**
   * Remove val from the domain. If this variable is instantiated, linked propagators are called.
   * @param value
   * @throws EmptyDomainException: if the domain becomes empty
   */
  override def removeValue(value: Int): Unit = {
    if (content.size == 1 && hasValue(value))
      throw new EmptyDomainException()
    content -= value
  }

  /**
   * Test if a value is in the domain
   * @param value: value to test
   * @return  true if the domain contains the value val, false otherwise
   */
  override def hasValue(value: Int): Boolean = content.contains(value)

  /**
   * Returns a copy of the same type as the current one
   * @return
   */
  override def copy(): SetDomainStorage = new SetDomainStorage(content.clone())
}