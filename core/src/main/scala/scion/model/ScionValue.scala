package scion.model

import better.files.File

sealed trait ScionValue {
  def value: Any

  def scionType: ScionType
}

object ScionValue {

  case class FileValue(value: File) extends ScionValue {
    override def scionType: ScionType = ScionType.file
  }

  case class ContentValue(value: String) extends ScionValue {
    override def scionType: ScionType = ScionType.content
  }

  case class StringValue(value: String) extends ScionValue {
    override def scionType: ScionType = ScionType.string
  }

  object UnitValue extends ScionValue {
    override def value: Any = ()

    override def scionType: ScionType = ScionType.unit
  }

}
