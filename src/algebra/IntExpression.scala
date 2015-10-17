package algebra

import misc.{EmptyDomainException, VariableNotBoundException}
import models.ModelDeclaration
import vars.IntVar

/**
 * An expression that represents an Integer
 */
trait IntExpression extends Iterable[Int] {
  /**
   * Evaluate this expression. All variables referenced have to be bound.
   * @throws VariableNotBoundException when a variable is not bound
   * @return the value of this expression
   */
  def evaluate(): Int

  /**
   * Return a lower bound for this expression
   */
  def min: Int

  /**
   * Return a higher bound for this expression
   */
  def max: Int

  /**
   * Get an iterator to all the values that this expression can take
   */
  def iterator: Iterator[Int]
  override def foreach[@specialized(Int) U](f: Int => U): Unit = iterator.foreach(f)

  /**
   * Give a variable that is equal to this expression. May post appropriate constraints.
   * @param modelDeclaration the ModelDeclaration object in which new variable/constraint will be created
   * @throws EmptyDomainException when the new IntVar has an empty domain
   * @return an IntVar
   */
  def reify()(implicit modelDeclaration: ModelDeclaration): IntVar = {
    val z = IntVar(min, max)(modelDeclaration)
    modelDeclaration.post(new Eq(this, z))
    z
  }

  def + (b: IntExpression): IntExpression = new BinarySum(this, b)
  def - (b: IntExpression): IntExpression = new Minus(this, b)
  def * (b: IntExpression): IntExpression = new Prod(this, b)
  def / (b: IntExpression): IntExpression = new Div(this, b)
  def % (b: IntExpression): IntExpression = new Modulo(this, b)
  def ~** (b: IntExpression): IntExpression = new Exponent(this, b)
  def ~^ (b: IntExpression): IntExpression = new Exponent(this, b)
  def == (b: IntExpression): BoolExpression = new Eq(this, b)
  def == (b: Int): BoolExpression = new Eq(this, b)
  def === (b: IntExpression): BoolExpression = new Eq(this, b)
  def != (b: IntExpression): BoolExpression = new NotEq(this, b)
  def >= (b: IntExpression): BoolExpression = new GrEq(this, b)
  def > (b: IntExpression): BoolExpression = new Eq(this, b)
  def <= (b: IntExpression): BoolExpression = new LrEq(this, b)
  def < (b: IntExpression): BoolExpression = new Lr(this, b)
  def in (b: Set[Int]): BoolExpression = new InSet(this, b)
  def unary_- : IntExpression = new UnaryMinus(this)
  def unary_+ : IntExpression = this
  def unary_! : IntExpression = this != 1
}

object IntExpression
{
  implicit def constant(v: Int): Constant = new Constant(v)
  implicit def array_element[A <: IntExpression](v: Array[A]): ArrayIntExpressionElementConstraintBuilder = new ArrayIntExpressionElementConstraintBuilder(v.asInstanceOf[Array[IntExpression]])
  implicit def array_element(v: Array[Int]): ArrayIntExpressionElementConstraintBuilder = new ArrayIntExpressionElementConstraintBuilder(v.map(new Constant(_)))

  class ArrayIntExpressionElementConstraintBuilder(val array: Array[IntExpression]) {
    def apply(id: IntExpression): IntExpression = new Element(array, id)
  }
}