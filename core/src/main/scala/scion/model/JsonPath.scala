package scion.model

import scion.model.JsonPath.Element

final case class JsonPath(elements: Seq[Element]) {
  def /(element: Element): JsonPath = JsonPath(elements :+ element)

  def /(oPath: JsonPath): JsonPath = JsonPath(elements ++ oPath.elements)

  def isParentOf(oPath: JsonPath): Boolean = oPath.elements.startsWith(elements)

  def isChildOf(oPath: JsonPath): Boolean = elements.startsWith(oPath.elements)

  def getFromThisTo(oPath: JsonPath): Option[JsonPath] = {
    if (isParentOf(oPath)) {
      Some(JsonPath(oPath.elements.drop(elements.size)))
    } else {
      None
    }
  }
}

object JsonPath {

  sealed trait Element

  final case class KeyElement(key: String) extends Element

  final case class IndexElement(index: Long) extends Element

  val root = JsonPath(Seq.empty)
}