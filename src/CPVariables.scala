abstract class CPVar {
  def print: String
}

abstract class CPIntVar extends CPVar {}

class CPAdaptableIntVar(minValue: Int, maxValue: Int) extends CPIntVar
{
  def print = "" + minValue + " " + maxValue
}