package misc

import java.io.{IOException, ObjectInputStream, ObjectOutputStream}
import scala.util.DynamicVariable
import models.Model


@SerialVersionUID(13l)
class DynamicModelVariable() extends Serializable {
  private var v = new DynamicVariable[Model](null)

  def value: Model = v.value

  def withValue[S](newval: Model)(thunk: => S): S = {
    v.withValue(newval)(thunk)
  }

  def value_=(newval: Model) = {
    v.value = newval
  }

  override def toString: String = "DynamicModelVariable(" + value + ")"

  @throws(classOf[IOException])
  private def writeObject(out: ObjectOutputStream): Unit = {
    out.writeObject(this.value)
  }

  @throws(classOf[IOException])
  private def readObject(in: ObjectInputStream): Unit = {
    v = new DynamicVariable[Model](in.readObject().asInstanceOf[Model])
  }
}
