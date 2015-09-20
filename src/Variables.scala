/**
 * The top class for any model variable
 * @param model: the model associated with this Var
 */
abstract class Var(val model: Model) {
  model.register_variable(this)
}


//Model of IntVar
abstract class IntVar(model: Model) extends Var(model) {}

object IntVar {
  def apply(minValue: Int, maxValue: Int)(implicit model: Model) = {
    new AdaptableIntVar(model, minValue, maxValue)
  }
}

class AdaptableIntVar(model: Model, minValue: Int, maxValue: Int) extends IntVar(model) {}