package misc

import scala.collection.mutable.ArrayBuffer

object UnionFindStorage {
  def apply[StoredType]() = new UnionFindStorage[StoredType](new ArrayBuffer[Elem[StoredType]])

  /**
   * Init this UnionFindStorage with a copy of the content of another one, with a type conversion
   * @param to_copy: UnionFindStorage to copy
   * @param convert: Convert function, gives instance of type StoredType for old objects
   */
  def apply[StoredType, OldType](to_copy: UnionFindStorage[OldType], convert: OldType => StoredType) = {
    val darray = to_copy.array
    val new_array = to_copy.array.map((elem: Elem[OldType]) => {
      if (elem.isInstanceOf[ElemLink[OldType]])
        new ElemLink[StoredType](elem.pos, darray(elem.pos).get(darray).pos)
      else
        new ElemMain[StoredType](elem.pos, convert(elem.asInstanceOf[ElemMain[OldType]].getContent))
    })
    new UnionFindStorage[StoredType](new_array)
  }
}

/**
 * A (semi-mutable union-find implementation that does objet-storage (each set have one object)
 * The union operation is NOT commutative!
 * Constraint: the number of entries can only increase
 */
//TODO: should provide a thread-safe implem, this is not the case for now
class UnionFindStorage[StoredType](private val array: ArrayBuffer[Elem[StoredType]]) {
  /**
   * Add a new element to the union-find, creating a new set
   * @param elem
   * @return
   */
  def add(elem: StoredType): Int = {
    val newid = array.length
    array.append(new ElemMain[StoredType](newid, elem))
    newid
  }

  /**
   * Add a new element to the union-find, adding it to another set
   * @param as_set_member
   * @return
   */
  def add(as_set_member: Int): Int = {
    val newid = array.length
    array.append(new ElemLink(newid, as_set_member))
    newid
  }

  /**
   * Do a union. It will drop the value of dropthis, and point it to usethis.
   * @param dropthis
   * @param usethis
   */
  def union(dropthis: Int, usethis: Int) = {
    val rd = array(dropthis).get(array).pos
    val ru = array(usethis).get(array).pos
    array(rd) = new ElemLink(rd, ru)
  }

  /**
   * Find the contained value
   */
  def find(setid: Int): StoredType = array(setid).get(array).getContent

  /**
   * Return the number of elements (NOT the number of set)
   * @return
   */
  def size: Int = array.size
}

private abstract class Elem[StoredType](val pos: Int) {
  def get(array: ArrayBuffer[Elem[StoredType]]): ElemMain[StoredType]
}

private class ElemMain[StoredType](pos: Int, private var elem: StoredType) extends Elem[StoredType](pos) {
  def get(array: ArrayBuffer[Elem[StoredType]]): ElemMain[StoredType] = this

  def getContent: StoredType = elem

  def setContent(newval: StoredType) = elem = newval
}

private class ElemLink[StoredType](pos: Int, private var to: Int) extends Elem[StoredType](pos) {
  def get(array: ArrayBuffer[Elem[StoredType]]): ElemMain[StoredType] = {
    val top = array(to).get(array)
    to = top.pos
    top
  }
}