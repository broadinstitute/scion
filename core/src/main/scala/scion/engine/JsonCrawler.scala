package scion.engine

import io.circe.Json

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
    def parentOpt: Option[JsonWithAnchor]
  }

  object Anchor {
    val root: Anchor = Root

    def inArray(jsonWithAnchor: JsonWithAnchor, index: Long): Anchor = ArrayMembership(jsonWithAnchor, index)

    def inObject(jsonWithAnchor: JsonWithAnchor, key: String): Anchor = ObjectMembership(jsonWithAnchor, key)
  }

  object Root extends Anchor {
    override def parentOpt: None.type = None
  }

  sealed trait Membership extends Anchor {
    override def parentOpt: Some[JsonWithAnchor] = Some(parent)

    def parent: JsonWithAnchor
  }

  case class ArrayMembership(parent: JsonWithAnchor, index: Long) extends Membership

  case class ObjectMembership(parent: JsonWithAnchor, key: String) extends Membership

  case class JsonWithAnchor(json: Json, anchor: Anchor)

}