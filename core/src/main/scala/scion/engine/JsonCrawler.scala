package scion.engine

import io.circe.Json
import scion.model.JsonPath

object JsonCrawler {

  def crawl(json: Json, anchor: Anchor = Anchor.root): Iterator[JsonWithAnchor] = {
    val jsonWithAnchor = JsonWithAnchor(json, anchor)
    val jsonWithAnchorIter = Iterator.single(jsonWithAnchor)
    if (json.isArray) {
      val childrenIter = json.asArray.get.iterator.zipWithIndex.flatMap { childWithIndex =>
        val (child, index) = childWithIndex
        val anchor = Anchor.inArray(jsonWithAnchor, index)
        crawl(child, anchor)
      }
      jsonWithAnchorIter ++ childrenIter
    } else if (json.isObject) {
      val childrenIter = json.asObject.get.toList.flatMap { keyAndChild =>
        val (key, child) = keyAndChild
        val anchor = Anchor.inObject(jsonWithAnchor, key)
        crawl(child, anchor)
      }
      jsonWithAnchorIter ++ childrenIter
    } else {
      jsonWithAnchorIter
    }
  }

  sealed trait Anchor {
    def pathElementOpt: Option[JsonPath.Element]
    def parentOpt: Option[JsonWithAnchor]
    def path: JsonPath
  }

  object Anchor {
    val root: Anchor = Root

    def inArray(jsonWithAnchor: JsonWithAnchor, index: Long): Anchor =
      Membership(jsonWithAnchor, JsonPath.IndexElement(index))

    def inObject(jsonWithAnchor: JsonWithAnchor, key: String): Anchor =
      Membership(jsonWithAnchor, JsonPath.KeyElement(key))
  }

  object Root extends Anchor {
    override def parentOpt: None.type = None

    override def pathElementOpt: None.type = None

    override def path: JsonPath = JsonPath.root
  }

  final case class Membership(parent: JsonWithAnchor, pathElement: JsonPath.Element) extends Anchor {
    override def parentOpt: Some[JsonWithAnchor] = Some(parent)

    override def pathElementOpt: Option[JsonPath.Element] = Some(pathElement)

    override def path: JsonPath = parent.anchor.path / pathElement
  }

  final case class JsonWithAnchor(json: Json, anchor: Anchor) {
    def path: JsonPath = anchor.path
  }

}