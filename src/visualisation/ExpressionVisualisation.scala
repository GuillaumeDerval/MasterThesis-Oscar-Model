package visualisation

import org.graphstream.ui.view.{Viewer, ViewerListener}

import scala.collection.JavaConversions._

import algebra._
import constraints.{ExpressionConstraint, Constraint}
import org.graphstream.graph.{Node, Edge}
import org.graphstream.graph.implementations.SingleGraph
import vars.{BoolVar, IntVar}
import visualisation.Color.Color

object Color extends Enumeration {
  type Color = Value
  val DEFAULT, SPECIAL = Value
}

class ConstraintsVisualisation(constraints: Array[Constraint]) {
  System.setProperty("org.graphstream.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer")
  val graph = new SingleGraph("Constraint view")
  var nextidx: Int = 0

  graph.setAttribute("ui.stylesheet",
    """
      graph {
      }
      node {
        size-mode: fit;
        shape: box;
        fill-color: transparent;
        stroke-mode: plain;
        stroke-color: yellow;
        text-background-mode: rounded-box;
        text-visibility-mode: normal;
        text-background-color: #ddd;
        text-alignment: center;
        text-padding: 5px;
      }
      node.special {
        text-background-color: #c77;
      }
      edge {
        text-alignment: along;
        fill-color: #ccc;
      }
      edge.special {
        fill-color: #c77;
      }
    """)
  val roots = constraints.map(parseConstraint(_))


  def display(): Unit = {
    val viewer = graph.display(false)
    val layout = new ForestLayout
    viewer.enableAutoLayout(layout)
    layout.setRoots(roots.toList)
    new TerminalNodeDisplay(viewer).pump()
  }

  private def parseConstraint(const: Constraint): String = {
    const match {
      case ExpressionConstraint(expr) => createRoot(const, expr)
    }
  }

  private def createRoot(const: Constraint, expr: IntExpression): String = {
    //TODO: create a specific node for the constraint
    //TODO: each constraint may have more than one sub-expr
    parseExpression(expr)
  }

  private def parseExpression(expr: IntExpression): String = {
    expr match {
      //std expressions
      case And(array) => basicCreate(expr, "And", array, "&&")
      case BinaryAnd(a, b) => basicCreate(expr, "And (Binary)", Seq(a,b), "&&")
      case BinaryOr(a, b) => basicCreate(expr, "Or (Binary)", Seq(a,b), "||")
      case BinarySum(a, b) => basicCreate(expr, "Sum (Binary)", Seq(a,b), "+")
      case Div(x, y) => basicCreate(expr, "Divide", Seq(x,y), "/")
      case Eq(a, b) => basicCreate(expr, "Equality", Seq(a,b), "=")
      case Exponent(x, y) => basicCreate(expr, "Exponent", Seq(x,y), "**")
      case Gr(a, b) => basicCreate(expr, "Greater", Seq(a,b), ">")
      case GrEq(a, b) => basicCreate(expr, "Greater or equal", Seq(a,b), ">=")
      case Implication(a,b) => basicCreate(expr, "Implication", Seq(a,b), "=>")
      case Lr(a, b) => basicCreate(expr, "Lesser", Seq(a,b), "<")
      case LrEq(a, b) => basicCreate(expr, "Lesser or equal", Seq(a,b), "<=")
      case Max(a) => basicCreate(expr, "Max", a, "")
      case Min(a) => basicCreate(expr, "Min", a, "")
      case Minus(a, b) => basicCreate(expr, "Minus", Seq(a,b), "-")
      case Modulo(a, b) => basicCreate(expr, "Modulo", Seq(a,b), "%")
      case Not(a) => basicCreate(expr, "Not", Seq(a), "")
      case NotEq(a, b) => basicCreate(expr, "Not equal", Seq(a, b), "!=")
      case Or(a) => basicCreate(expr, "Or", a, "||")
      case Prod(a, b) => basicCreate(expr, "Product", Seq(a,b), "*")
      case Sum(a) => basicCreate(expr, "Sum", a, "+")
      case UnaryMinus(a) => basicCreate(expr, "Unary minus", Seq(a), "")
      case Xor(a, b) => basicCreate(expr, "Xor", Seq(a,b), "^")

      //terminal expressions
      case Constant(a) => terminalNodeCreate(expr, "Constant ("+a.toString+")")
      case i: BoolVar => terminalNodeCreate(expr, "BoolVar")
      case i: IntVar => terminalNodeCreate(expr, "IntVar")

      //More complicated expressions
      case Count(array, value) =>
        val t1 = array.zipWithIndex.map(entry => (parseExpression(entry._1), "array["+entry._2+"]", Color.DEFAULT))
        val t2 = Array((parseExpression(value), "value", Color.SPECIAL))
        val merge = t1 ++ t2
        nodeCreate(expr, "Count", merge, Array.tabulate(merge.size-1)(_ => ""), Color.DEFAULT)
      case Element(array, key) =>
        val t1 = array.zipWithIndex.map(entry => (parseExpression(entry._1), "array["+entry._2+"]", Color.DEFAULT))
        val t2 = Array((parseExpression(key), "key", Color.SPECIAL))
        val merge = t1 ++ t2
        nodeCreate(expr, "Element", merge, Array.tabulate(merge.size-1)(_ => ""), Color.DEFAULT)
      case InSet(value, set) =>
        val t1 = set.toArray.map(entry => (parseExpression(entry), "set element", Color.DEFAULT))
        val t2 = Array((parseExpression(value), "value", Color.SPECIAL))
        val merge = t1 ++ t2
        nodeCreate(expr, "In set", merge, Array.tabulate(merge.size-1)(_ => ""), Color.DEFAULT)
      case WeightedSum(array, weights) =>
        val t = array.zip(weights).map(entry => (parseExpression(entry._1), entry._2.toString, Color.DEFAULT))
        nodeCreate(expr, "Weighted Sum", t, Array.tabulate(t.size-1)(_ => ""), Color.DEFAULT)
    }
  }

  private def basicCreate(expr: IntExpression, name: String, children: Seq[IntExpression], between: Seq[String]): String = nodeCreate(expr, name, children.map(a => (parseExpression(a), "", Color.DEFAULT)), between, Color.DEFAULT)
  private def basicCreate(expr: IntExpression, name: String, children: Seq[IntExpression], between: String): String = basicCreate(expr, name, children, Array.tabulate(children.size-1)(_ => between))
  private def terminalNodeCreate(expr: IntExpression, name: String): String = nodeCreate(expr, name, Seq(), Seq(), Color.SPECIAL)

  private def nodeCreate(expr: IntExpression, name: String, children: Seq[(String, String, Color)], between: Seq[String], color: Color): String = {
    val idx = nextidx.toString
    nextidx += 1
    val node: Node = graph.addNode(idx)
    node.addAttribute("ui.label", name)
    node.addAttribute("expression", expr)

    color match {
      case Color.DEFAULT => {}//do nothing
      case Color.SPECIAL => node.setAttribute("ui.class", "special")
    }

    for(((child, edgeName, color), cidx) <- children.zipWithIndex) {
      val edge: Edge = graph.addEdge(idx+"-"+cidx+"-"+child, child, idx, true)
      if(edgeName != "")
        edge.setAttribute("ui.label", edgeName)
      color match {
        case Color.DEFAULT => {}//do nothing
        case Color.SPECIAL => edge.setAttribute("ui.class", "special")
      }
    }
    idx
  }

  class TerminalNodeDisplay(viewer: Viewer) extends ViewerListener {
    var stop = false
    val pipe = viewer.newViewerPipe()
    pipe.addViewerListener(this)
    pipe.addSink(graph)

    def pump() { while(!stop) pipe.blockingPump() }
    override def buttonReleased(id: String): Unit = {
      val n:Node = graph.getNode(id)
      val expr: IntExpression= n.getAttribute("expression")

      expr match {
        case a: BoolVar =>
          println("Var id: "+a.varid)
          println("\tBoolean value: "+a.iterator.mkString(","))
        case a: IntVar =>
          println("Var id: "+a.varid)
          println("\tInteger value: "+a.iterator.mkString(","))
        case Constant(a) =>
          println("Constant value: "+a.toString)
        case default => {}
      }
    }

    override def buttonPushed(id: String): Unit = {}

    override def viewClosed(viewName: String): Unit = {
      stop = true
    }
  }
}

