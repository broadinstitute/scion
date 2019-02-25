package scion.model

case class ScionFunctionSignature(name: String, returnType: ScionType, arguments: Map[String, ScionType],
                                  mandatory: Set[Set[String]]) {

}

