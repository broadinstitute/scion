package scion.model

case class ScionFunction(name: String, returnType: ScionType, arguments: Map[String, ScionType],
                         mandatory: Set[Set[String]]) {

}

