package scion.engine

import io.circe.Json
import scion.engine.JsonCrawler.{Analysis, ArrayMembership, Context, Membership, ObjectMembership}

class JsonCrawler[V](inspector: Context[V] => Analysis[V], recorder: Analysis[V] => Unit) {

  def crawl(json: Json): Analysis[V] = crawl(json, None)

  def crawl(json: Json, parent: Analysis[V], index: Long): Analysis[V] = crawl(json, Some(ArrayMembership(parent, index)))

  def crawl(json: Json, parent: Analysis[V], key: String): Analysis[V] = crawl(json, Some(ObjectMembership(parent, key)))

  def crawl(json: Json, parentOpt: Option[Membership[V]]): Analysis[V] = {
    val context = Context(json, parentOpt)
    val analysis = inspector(context)
    recorder(analysis)
    if(json.isArray) {
      ??? // TODO
    } else if(json.isObject) {
      ??? // TODO
    }
    analysis
  }

}

object JsonCrawler {

  trait Membership[V] {
    def parent: Analysis[V]
  }

  case class ArrayMembership[V](parent: Analysis[V], index: Long) extends Membership[V]
  case class ObjectMembership[V](parent: Analysis[V], key: String) extends Membership[V]

  case class Context[V](json: Json, parentOpt: Option[Membership[V]])
  case class Analysis[V](value: V, context: Context[V])

}