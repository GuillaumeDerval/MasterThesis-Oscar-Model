package vars.domainstorage.int

import vars.IntVarImplem
import vars.domainstorage.DomainStorage

import scala.collection.mutable

trait IntDomainStorage extends DomainStorage with IntVarImplem
{
  /**
   * Returns a copy of the same type as the current one
   * @return
   */
  override def copy(): IntDomainStorage
}

object IntDomainStorage {
  /**
   * Create a new IntDomainStorage from a Set of values
   * @param s
   */
  def apply(s: Set[Int]) = new AdaptableIntDomainStorage(new SetDomainStorage(mutable.SortedSet(s.toList: _*)))

  /**
   * Create a new IntDomainStorage from a Set of values
   * @param s
   */
  def apply(s: mutable.SortedSet[Int]) = new AdaptableIntDomainStorage(new SetDomainStorage(s))

  /**
   * Create a new IntDomainStorage containing only a single value
   * @param v
   */
  def apply(v: Int) = new AdaptableIntDomainStorage(new SingletonDomainStorage(v))

  /**
   * Create a new IntDomainStorage, with domain from min to max, inclusive
   * @param min
   * @param max
   */
  def apply(min: Int, max: Int) = new AdaptableIntDomainStorage(new IntervalDomainStorage(min, max))
}