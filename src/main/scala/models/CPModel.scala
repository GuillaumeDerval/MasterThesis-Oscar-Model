package models

import algebra._
import constraints._
import oscar.algo.reversible.ReversibleInt
import oscar.cp
import oscar.cp.constraints.{CPObjective, CPObjectiveUnit, CPObjectiveUnitMaximize, CPObjectiveUnitMinimize}
import oscar.cp.core.{CPOutcome, CPPropagStrength}
import oscar.cp.{CPBoolVarOps, CPIntVarOps}
import vars.cp.int.{CPBoolVar, CPIntVar}
import vars.domainstorage.int.{AdaptableIntDomainStorage, IntervalDomainStorage, SetDomainStorage, SingletonDomainStorage}
import vars.{BoolVar, IntVar}

/**
  * Created by dervalguillaume on 12/04/16.
  */
class CPModel(p: UninstantiatedModel) extends InstantiatedModel(p){
  implicit lazy val cpSolver = new oscar.cp.CPSolver()
  override type IntVarImplementation = CPIntVar

  val cpObjective: CPObjectiveUnit= this.optimisationMethod match {
    case m: Minimisation =>
      new CPObjectiveUnitMinimize(this.getRepresentative(m.objective).realCPVar)
    case m: Maximisation =>
      new CPObjectiveUnitMaximize(this.getRepresentative(m.objective).realCPVar)
    case _ => null
  }

  if(cpObjective != null)
    cpSolver.optimize(new CPObjective(cpSolver, cpObjective))

  def getReversibleInt(init: Int) = new ReversibleInt(cpSolver, init)

  override protected def instantiateAdaptableIntDomainStorage(adaptable: AdaptableIntDomainStorage): CPIntVar = {
    if(adaptable.min >= 0 && adaptable.max <= 1)
      CPBoolVar(adaptable, cpSolver)
    else
      CPIntVar(adaptable.content, cpSolver)
  }

  override protected def instantiateSetDomainStorage(set: SetDomainStorage): CPIntVar = {
    if(set.min >= 0 && set.max <= 1)
      CPBoolVar(set, cpSolver)
    else
      CPIntVar(set, cpSolver)
  }

  override protected def instantiateSingletonDomainStorage(singleton: SingletonDomainStorage): CPIntVar = {
    if(singleton.min >= 0 && singleton.max <= 1)
      CPBoolVar(singleton, cpSolver)
    else
      CPIntVar(singleton, cpSolver)
  }

  override protected def instantiateIntervalDomainStorage(interval: IntervalDomainStorage): CPIntVar = {
    if(interval.min >= 0 && interval.max <= 1)
      CPBoolVar(interval, cpSolver)
    else
      CPIntVar(interval, cpSolver)
  }

  override def post(constraint: Constraint): Boolean = {
    constraint match {
      case ExpressionConstraint(expr: BoolExpression) => postBooleanExpression(expr)
      case AllDifferent(array) => cpSolver.post(cp.modeling.constraint.allDifferent(array.map(postIntExpressionAndGetVar)), CPPropagStrength.Weak) != CPOutcome.Failure
      case Table(array, values) => cpSolver.post(cp.modeling.constraint.table(array.map(postIntExpressionAndGetVar), values)) != CPOutcome.Failure
      case MinCircuit(succ, distMatrixSucc, cost) => cpSolver.post(cp.modeling.constraint.minCircuit(succ.map(postIntExpressionAndGetVar), distMatrixSucc, postIntExpressionAndGetVar(cost)), CPPropagStrength.Strong) != CPOutcome.Failure
      case GCC(x, minVal, low, up) => cpSolver.post(new oscar.cp.constraints.GCC(x.map(postIntExpressionAndGetVar), minVal, low, up)) != CPOutcome.Failure
      case BinPacking(x, w, l) => cpSolver.post(new cp.constraints.BinPacking(x.map(postIntExpressionAndGetVar), w, l.map(postIntExpressionAndGetVar))) != CPOutcome.Failure
      case Circuit(succ, symmetric) => cpSolver.post(new cp.constraints.Circuit(succ.map(postIntExpressionAndGetVar), symmetric), CPPropagStrength.Strong) != CPOutcome.Failure
      case Inverse(a, b) => cpSolver.post(new cp.constraints.Inverse(a.map(postIntExpressionAndGetVar), b.map(postIntExpressionAndGetVar))) != CPOutcome.Failure
      case MinAssignment(xarg, weightsarg, cost) => cpSolver.post(new cp.constraints.MinAssignment(xarg.map(postIntExpressionAndGetVar), weightsarg, postIntExpressionAndGetVar(cost))) != CPOutcome.Failure
      case default => throw new Exception() //TODO: put a real exception here
    }
  }

  def postBooleanExpression(expr: BoolExpression): Boolean = {
    expr match {
      case And(array) =>
        array.forall(i => postBooleanExpression(i))
      case BinaryAnd(a, b) =>
        postBooleanExpression(a)
        postBooleanExpression(b)
      case BinaryOr(a, b) =>
        cpSolver.add(oscar.cp.or(Array(postBoolExpressionAndGetVar(a),postBoolExpressionAndGetVar(b)))) != CPOutcome.Failure
      case Eq(a, b) =>
        postConstraintForPossibleConstant(a, b,
          (x,y)=>(y == x),
          (x,y)=>(x == y),
          (x,y)=>(x == y)
        )
      case Gr(a, b) =>
        cpSolver.add(new oscar.cp.constraints.Gr(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b))) != CPOutcome.Failure
      case GrEq(a, b) =>
        cpSolver.add(new oscar.cp.constraints.GrEq(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b))) != CPOutcome.Failure
      case Lr(a, b) =>
        cpSolver.add(new oscar.cp.constraints.Le(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b))) != CPOutcome.Failure
      case LrEq(a, b) =>
        cpSolver.add(new oscar.cp.constraints.LeEq(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b))) != CPOutcome.Failure
      case Or(a) =>
        cpSolver.add(oscar.cp.or(a.map(postBoolExpressionAndGetVar))) != CPOutcome.Failure
      case Not(a) =>
        cpSolver.add(postBoolExpressionAndGetVar(a).not) != CPOutcome.Failure
      case NotEq(a, b) =>
        postConstraintForPossibleConstant(a, b,
          (x,y)=>(y != x),
          (x,y)=>(x != y),
          (x,y)=>(x != y)
        )
      case InSet(a, b) =>
        cpSolver.add(new cp.constraints.InSet(postIntExpressionAndGetVar(a), b)) != CPOutcome.Failure
      case Implication(a, b) =>
        val v = oscar.cp.CPBoolVar()
        cpSolver.add(new oscar.cp.constraints.Implication(v, postBoolExpressionAndGetVar(a), postBoolExpressionAndGetVar(b))) != CPOutcome.Failure &&
        cpSolver.add(v) != CPOutcome.Failure
      case Xor(a, b) => throw new Exception() //TODO: throw valid exception
      case v: BoolVar =>
        cpSolver.add(getRepresentative(v).realCPVar.asInstanceOf[oscar.cp.CPBoolVar])  != CPOutcome.Failure
    }
  }

  def postBoolExpressionAndGetVar(expr: BoolExpression): oscar.cp.CPBoolVar = {
    expr match {
      case And(array) =>
        array.foldLeft(oscar.cp.CPBoolVar())((a,b) => CPBoolVarOps(a) && postBoolExpressionAndGetVar(b))
      case BinaryAnd(a, b) =>
        CPBoolVarOps(postBoolExpressionAndGetVar(a)) && postBoolExpressionAndGetVar(b)
      case BinaryOr(a, b) =>
        CPBoolVarOps(postBoolExpressionAndGetVar(a)) || postBoolExpressionAndGetVar(b)
      case Eq(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) === postIntExpressionAndGetVar(b)
      case Gr(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) >>= postIntExpressionAndGetVar(b)
      case GrEq(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) >== postIntExpressionAndGetVar(b)
      case Lr(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) <<= postIntExpressionAndGetVar(b)
      case LrEq(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) <== postIntExpressionAndGetVar(b)
      case Or(array) =>
        array.foldLeft(oscar.cp.CPBoolVar(false))((a,b) => CPBoolVarOps(a) || postBoolExpressionAndGetVar(b))
      case Not(a) =>
        postBoolExpressionAndGetVar(a).not
      case NotEq(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) !== postIntExpressionAndGetVar(b)
      case Implication(a, b) =>
        CPBoolVarOps(postBoolExpressionAndGetVar(a)) ==> postBoolExpressionAndGetVar(b)
      case Xor(a, b) => throw new Exception() //TODO: throw valid exception
      case InSet(a, b) => throw new Exception() //TODO: throw valid exception
      case v: BoolVar => getRepresentative(v).realCPVar.asInstanceOf[oscar.cp.CPBoolVar]
    }
  }

  def postIntExpressionAndGetVar(expr: IntExpression): oscar.cp.CPIntVar = {
    expr match {
      case expr: BoolExpression => postBoolExpressionAndGetVar(expr)
      case Constant(a) => oscar.cp.CPIntVar(a)
      case Count(x, y) =>
        val v = oscar.cp.CPIntVar(0, x.length)
        cpSolver.add(new oscar.cp.constraints.Count(v, x.map(postIntExpressionAndGetVar), postIntExpressionAndGetVar(y)))
        v
      case Element(x, y) =>
        val vx: Array[oscar.cp.CPIntVar] = x.map(postIntExpressionAndGetVar)
        val vy: oscar.cp.CPIntVar = postIntExpressionAndGetVar(y)
        vx(vy)
      case Max(x) =>
        val vx = x.map(postIntExpressionAndGetVar)
        val m = oscar.cp.CPIntVar(vx.map(_.min).max, vx.map(_.max).max)
        cpSolver.add(oscar.cp.maximum(vx, m))
        m
      case Min(x) =>
        val vx = x.map(postIntExpressionAndGetVar)
        val m = oscar.cp.CPIntVar(vx.map(_.min).min, vx.map(_.max).min)
        cpSolver.add(oscar.cp.maximum(vx, m))
        m
      case Minus(x, y) =>
        getCPIntVarForPossibleConstant(x, y,
          (a,b) => (-b + a),
          (a,b) => (a - b),
          (a,b) => (a - b))
      case Modulo(x, y) =>
        val v = oscar.cp.CPIntVar(expr.min, expr.max)
        cpSolver.add(new CPIntVarOps(postIntExpressionAndGetVar(x)) % y == v)
        v
      case Prod(x, y) =>
        getCPIntVarForPossibleConstant(x, y,
          (a,b) => b * a,
          (a,b) => a * b,
          (a,b) => a * b)
      case Sum(x) =>
        oscar.cp.sum(x.map(postIntExpressionAndGetVar))
      case BinarySum(x, y) =>
        getCPIntVarForPossibleConstant(x, y,
          (a,b) => (b + a),
          (a,b) => (a + b),
          (a,b) => (a + b))
      case UnaryMinus(a) =>
        -postIntExpressionAndGetVar(a)
      case WeightedSum(x, y) =>
        oscar.cp.weightedSum(y, x.map(postIntExpressionAndGetVar))
      case Div(x, y) => throw new Exception() //TODO: real exception
      case Exponent(x, y) => throw new Exception() //TODO: real exception
      case v: IntVar =>
        getRepresentative(v).realCPVar
    }
  }

  /**
    * Post the right constraint depending on the type of a and b.
    *
    * @param a
    * @param b
    * @param leftCst this function will be called if a is constant
    * @param rightCst this function will be called if b is constant
    * @param allVar this function will be called if a and b are not constant
    */
  def postConstraintForPossibleConstant(a: IntExpression, b: IntExpression,
                                        leftCst: (Int, oscar.cp.CPIntVar) => oscar.cp.Constraint,
                                        rightCst: (oscar.cp.CPIntVar, Int) => oscar.cp.Constraint,
                                        allVar: (oscar.cp.CPIntVar, oscar.cp.CPIntVar) => oscar.cp.Constraint): Boolean = {
    (a,b) match {
      case (Constant(value), variable:IntExpression) => cpSolver.add(leftCst(value, postIntExpressionAndGetVar(variable))) != CPOutcome.Failure
      case (variable: IntExpression, Constant(value)) => cpSolver.add(rightCst(postIntExpressionAndGetVar(variable), value)) != CPOutcome.Failure
      case (v1: IntExpression, v2: IntExpression) => cpSolver.add(allVar(postIntExpressionAndGetVar(v1), postIntExpressionAndGetVar(v2))) != CPOutcome.Failure
    }
  }

  /**
    * Post the right constraint depending on the type of a and b.
    *
    * @param a
    * @param b
    * @param leftCst this function will be called if a is constant
    * @param rightCst this function will be called if b is constant
    * @param allVar this function will be called if a and b are not constant
    */
  def getCPIntVarForPossibleConstant(a: IntExpression, b: IntExpression,
                                     leftCst: (Int, oscar.cp.CPIntVar) => oscar.cp.CPIntVar,
                                     rightCst: (oscar.cp.CPIntVar, Int) => oscar.cp.CPIntVar,
                                     allVar: (oscar.cp.CPIntVar, oscar.cp.CPIntVar) => oscar.cp.CPIntVar): oscar.cp.CPIntVar = {
    (a,b) match {
      case (Constant(value), variable:IntExpression) => leftCst(value, postIntExpressionAndGetVar(variable))
      case (variable: IntExpression, Constant(value)) => rightCst(postIntExpressionAndGetVar(variable), value)
      case (v1: IntExpression, v2: IntExpression) => allVar(postIntExpressionAndGetVar(v1), postIntExpressionAndGetVar(v2))
    }
  }
}
