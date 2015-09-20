
/**
 * Exception launched when an invalid CPVar is used to implement a Var
 * @param msg: an explanation message
 */
class InvalidCPVarException(msg: String) extends IllegalArgumentException(msg)