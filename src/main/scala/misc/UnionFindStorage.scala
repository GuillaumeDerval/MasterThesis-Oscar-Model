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

  /**
    * Copy a UnionFindStorage
    * @param to_copy UnionFindStorage to be copied
    */
  def apply[StoredType](to_copy: UnionFindStorage[StoredType]) = {
    //Everything that is inside the array is immutable, so we just have to copy the array
    new UnionFindStorage[StoredType](to_copy.array.clone())
  }
}

/**
 * A (semi-mutable) union-find implementation that does object-storage (each set have one object)
 * Constraint: the number of entry can only increase
 */
class UnionFindStorage[StoredType](private val array: ArrayBuffer[Elem[StoredType]]) extends Serializable {
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
   * Do a union. It will drop the value of dropthis and dropThisToo, and point it to useThisInstead.
   * @param dropThis first element to be "merged"
   * @param dropThisToo second element to be "merged"
   * @param useThisInstead element that is the merge result of dropThis and dropThisToo
   */
  def union(dropThis: Int, dropThisToo: Int, useThisInstead: StoredType) = {
    val pos1 = array(dropThis   ).get(array).pos
    val pos2 = array(dropThisToo).get(array).pos
    array(pos1) = new ElemMain[StoredType](pos1, useThisInstead)
    array(pos2) = new ElemLink(pos2, pos1)
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

  /**
    * Iterate on the values really existing in this UF.
    * The first value of the tuple is the internal id of the StoredType.
    * @param f
    */
  def foreach(f : ((Int, StoredType)) => Unit) : Unit = {
    for((elem, idx) <- array.zipWithIndex; if elem.isInstanceOf[ElemMain[StoredType]])
        f((idx, elem.asInstanceOf[ElemMain[StoredType]].getContent))
  }
}

private abstract class Elem[StoredType](val pos: Int) extends Serializable {
  def get(array: ArrayBuffer[Elem[StoredType]]): ElemMain[StoredType]
}

private class ElemMain[StoredType](pos: Int, elem: StoredType) extends Elem[StoredType](pos) {
  def get(array: ArrayBuffer[Elem[StoredType]]): ElemMain[StoredType] = this

  def getContent: StoredType = elem
}

private class ElemLink[StoredType](pos: Int, to: Int) extends Elem[StoredType](pos) {
  def get(array: ArrayBuffer[Elem[StoredType]]): ElemMain[StoredType] = {
    val top = array(to).get(array)
    //Replace ourselve if we are not up-to-date
    if(top.pos != to)
      array(pos) = new ElemLink[StoredType](pos, top.pos)
    top
  }
}