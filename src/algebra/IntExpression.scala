package algebra

import misc.{EmptyDomainException, VariableNotBoundException}
import models.ModelDeclaration
import vars.IntVar
import vars.cp.int.CPIntVar

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
    modelDeclaration.post(new BoolExpressionEq(this, z))
    z
  }

  def + (b: IntExpression): IntExpression = new IntExpressionSum(this, b)
  def - (b: IntExpression): IntExpression = new IntExpressionMinus(this, b)
  def * (b: IntExpression): IntExpression = new IntExpressionProd(this, b)
  def / (b: IntExpression): IntExpression = new IntExpressionDiv(this, b)
  def % (b: IntExpression): IntExpression = new IntExpressionModulo(this, b)
  def ~** (b: IntExpression): IntExpression = new IntExpressionExponent(this, b)
  def ~^ (b: IntExpression): IntExpression = new IntExpressionExponent(this, b)
  def == (b: IntExpression): BoolExpression = new BoolExpressionEq(this, b)
  def == (b: Int): BoolExpression = new BoolExpressionEq(this, b)
  def === (b: IntExpression): BoolExpression = new BoolExpressionEq(this, b)
  def != (b: IntExpression): BoolExpression = new BoolExpressionNotEq(this, b)
  def >= (b: IntExpression): BoolExpression = new BoolExpressionGrEq(this, b)
  def > (b: IntExpression): BoolExpression = new BoolExpressionEq(this, b)
  def <= (b: IntExpression): BoolExpression = new BoolExpressionLrEq(this, b)
  def < (b: IntExpression): BoolExpression = new BoolExpressionLr(this, b)
  def in (b: Set[Int]): BoolExpression = new BoolExpressionInSet(this, b)
  def unary_- : IntExpression = new IntExpressionUnaryMinus(this)
  def unary_+ : IntExpression = this
  def unary_! : IntExpression = this != 1
}

object IntExpression
{
  implicit def constant(v: Int): IntExpressionConstant = new IntExpressionConstant(v)
  implicit def array_element[A <: IntExpression](v: Array[A]): ArrayIntExpressionElementConstraintBuilder = new ArrayIntExpressionElementConstraintBuilder(v.asInstanceOf[Array[IntExpression]])
  //implicit def array_element(v: Array[BoolExpression]): ArrayBoolExpressionElementConstraintBuilder = new ArrayBoolExpressionElementConstraintBuilder(v)
  implicit def array_element(v: Array[Int]): ArrayIntElementConstraintBuilder = new ArrayIntElementConstraintBuilder(v)


  class ArrayIntElementConstraintBuilder(val array: Array[Int]) {
    def apply(id: IntExpression): IntExpression = new IntExpressionElementCst(array, id)
  }

  class ArrayIntExpressionElementConstraintBuilder(val array: Array[IntExpression]) {
    def apply(id: IntExpression): IntExpression = new IntExpressionElement(array, id)
  }

  //class ArrayBoolExpressionElementConstraintBuilder(val array: Array[BoolExpression]) {
  //  def apply(id: IntExpression): BoolExpression = new IntExpressionElement(array.asInstanceOf[Array[IntExpression]], id) == 1
  //}
}