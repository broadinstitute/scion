package scion.engine

import io.circe.Json
import scion.engine.JsonCrawler.{Anchor, JsonWithAnchor}

class JsonCrawler(recorder: JsonWithAnchor => Unit) {

  def crawl(json: Json, anchor: Anchor = Anchor.root): JsonWithAnchor = {
    val jsonWithAnchor = JsonWithAnchor(json, anchor)
    recorder(jsonWithAnchor)
    if(json.isArray) {
      for((child, index) <- json.asArray.get.zipWithIndex) {
        val anchor = Anchor.inArray(jsonWithAnchor, index)
        crawl(child, anchor)
      }
    } else if(json.isObject) {
      for((key, child) <- json.asObject.get.toList) {
        val anchor = Anchor.inObject(jsonWithAnchor, key)
        crawl(child, anchor)
      }
    }
    jsonWithAnchor
  }

}

object JsonCrawler {

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