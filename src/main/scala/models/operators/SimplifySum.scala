package models.operators

import algebra._
import constraints.ExpressionConstraint
import models.uninstantiated.{ChildModel, UninstantiatedModel}

/**
 * Created by dervalguillaume on 22/10/15.
 */
object SimplifySum {
  def apply(model: UninstantiatedModel): UninstantiatedModel = {
    val newmodel= new ChildModel(model)

    newmodel.constraints.map((constraint) => {
      constraint match {
        case ExpressionConstraint(expr) => new ExpressionConstraint(SimplifySum(expr).asInstanceOf[BoolExpression])
        case default => constraint
      }
    })
    newmodel
  }

  def apply(expr: BoolExpression): BoolExpression = {
    SimplifySum(expr.asInstanceOf[IntExpression]).asInstanceOf[BoolExpression]
  }

  def apply(expr: IntExpression): IntExpression = {
    val s1 = convertToWeightedSum(expr)
    val s2 = updateWeightedSumCoefficient(s1)
    val s3 = mergeWeightedSums(s2)
    transformBackWeightedSum(s3)
  }

  private def convertToWeightedSum(expr: IntExpression): IntExpression = {
    val nexpr = expr.mapSubexpressions(convertToWeightedSum)
    nexpr match {
      case BinarySum(a, b) => WeightedSum(Array(a, b), Array(1, 1))
      case Sum(a) => WeightedSum(a, Array.tabulate(a.length)(_ => 1))
      case default => nexpr
    }
  }

  private def updateWeightedSumCoefficient(expr: IntExpression): IntExpression = {
    val nexpr = expr.mapSubexpressions(updateWeightedSumCoefficient)
    nexpr match {
      case WeightedSum(x, w) => {
        val nxw : Array[(IntExpression, Int)] = x.zip(w).map(updateWeightedSumCoefficientRecur)
        val (nx, nw) = nxw.unzip
        new WeightedSum(nx, nw)
      }
      case default => nexpr
    }
  }

  private def updateWeightedSumCoefficientRecur(ixw: (IntExpression, Int)): (IntExpression, Int) = {
    val ix = ixw._1
    val iw = ixw._2
    ix match {
      case Prod(sub, Constant(subw)) => updateWeightedSumCoefficientRecur((sub, subw*iw))
      case Prod(Constant(subw), sub) => updateWeightedSumCoefficientRecur((sub, subw*iw))
      case default => ixw
    }
  }

  private def mergeWeightedSums(expr: IntExpression): IntExpression = {
    val nexpr = expr.mapSubexpressions(mergeWeightedSums)
    nexpr match {
      case WeightedSum(x, w) => {
        val (nx, nw) = x.zip(w).flatMap(updateWeightedSum).unzip
        new WeightedSum(nx, nw)
      }
      case default => nexpr
    }
  }

  private def updateWeightedSum(ixw: (IntExpression, Int)): Array[(IntExpression, Int)] = {
    ixw._1 match {
      case WeightedSum(ix2, iw2) => ix2.zip(iw2.map(_*ixw._2))
      case default => Array(ixw)
    }
  }

  private def transformBackWeightedSum(expr: IntExpression): IntExpression = {
    val nexpr = expr.mapSubexpressions(transformBackWeightedSum)
    nexpr match {
      case WeightedSum(x, w) => {
        if(w.forall(v => v == 1)) {
          if(w.length == 2)
            new BinarySum(x(0), x(1))
          else
            new Sum(x)
        }
        else
          nexpr
      }
      case default => nexpr
    }
  }
}
