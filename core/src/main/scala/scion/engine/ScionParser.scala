package scion.engine

import io.circe.Json
import scion.model.{ScionDictionary, ScionFunction, ScionGraph, ScionNativeFunctions}
import scion.util.Result

class ScionParser {

  def getOptionalChild[V](json: Json, key: String, extractor: Json => Result[V]): Result[Option[V]] = {
    json.asObject match {
      case Some(jsonObject) =>
        jsonObject(key) match {
          case Some(childJson) =>
            val childResult = extractor(childJson)
            childResult.map(Some(_))
          case None => Result.forValue(None)
        }
      case None => Result.forValue(None)
    }
  }

  def jsonToTag(json: Json): Result[String] = {
    json.asString match {
      case Some(string) => Result.forValue(string)
      case None => Result.forErrorMessage(
        s"String expected, but got $json."
      )
    }
  }

  def jsonToFunction(json: Json): Result[ScionFunction] = {
    json.asString match {
      case Some(name) => ScionNativeFunctions.get(name)
      case None => Result.forErrorMessage(
        s"String expected, but got $json."
      )
    }
  }

  def getTagOpt(json: Json): Result[Option[String]] = getOptionalChild(json, ScionDictionary.tagKey, jsonToTag)

  def parse(json: Json): Result[ScionGraph] = {
    var result: Result[ScionGraph] = Result.forValue(ScionGraph.empty)
    for(jsonWithAnchor <- JsonCrawler.crawl(json)) {

    }


    result
  }

}

