package scion.engine

import io.circe.Json
import scion.engine.JsonCrawler.{Analysis, Anchor, Context}

class JsonCrawler[V](inspector: Context[V] => Analysis[V], recorder: Analysis[V] => Unit) {

  def crawl(json: Json, anchor: Anchor[V] = Anchor.root): Analysis[V] = {
    val context = Context(json, anchor)
    val analysis = inspector(context)
    recorder(analysis)
    if(json.isArray) {
      for((child, index) <- json.asArray.get.zipWithIndex) {
        val anchor = Anchor.inArray(analysis, index)
        crawl(child, anchor)
      }
    } else if(json.isObject) {
      for((key, child) <- json.asObject.get.toList) {
        val anchor = Anchor.inObject(analysis, key)
        crawl(child, anchor)
      }
    }
    analysis
  }

}

object JsonCrawler {

  sealed trait Anchor[+V] {
    def parentOpt: Option[Analysis[V]]
  }

  object Anchor {
    val root: Anchor[Nothing] = Root
    def inArray[V](analysis: Analysis[V], index: Long): Anchor[V] = ArrayMembership(analysis, index)
    def inObject[V](analysis: Analysis[V], key: String): Anchor[V] = ObjectMembership(analysis, key)
  }

  object Root extends Anchor[Nothing] {
    override def parentOpt: None.type = None
  }

  sealed trait Membership[+V] extends Anchor[V] {
    override def parentOpt: Some[Analysis[V]] = Some(parent)
    def parent: Analysis[V]
  }

  case class ArrayMembership[+V](parent: Analysis[V], index: Long) extends Membership[V]
  case class ObjectMembership[+V](parent: Analysis[V], key: String) extends Membership[V]

  case class Context[+V](json: Json, anchor: Anchor[V])
  case class Analysis[+V](value: V, context: Context[V])

}