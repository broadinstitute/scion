package scion.model

trait ScionFunction {

  def signature: ScionFunctionSignature

  def apply(arguments: Map[String, ScionValue]): ScionValue

}
