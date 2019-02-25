package scion.model

sealed trait ScionType {

}

object ScionType {

  case class Primitive(name: String) extends ScionType

  val boolean = Primitive("Boolean")
  val number = Primitive("Number")
  val string = Primitive("String")
  val file = Primitive("File")
  val content = Primitive("Content")
  val unit = Primitive("Unit")

  case class Result(primitive: Primitive) extends ScionType

}
