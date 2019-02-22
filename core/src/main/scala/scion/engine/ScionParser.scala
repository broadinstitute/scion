package scion.engine

import io.circe.Json
import scion.model.{ScionDictionary, ScionFunction, ScionGraph, ScionNativeFunctions}
import scion.util.ResultWithIssues

class ScionParser {

  def getOptionalChild[V](json: Json, key: String, extractor: Json => ResultWithIssues[V]): ResultWithIssues[Option[V]] = {
    json.asObject match {
      case Some(jsonObject) =>
        jsonObject(key) match {
          case Some(childJson) =>
            val childResult = extractor(childJson)
            childResult.map(Some(_))
          case None => ResultWithIssues.forValue(None)
        }
      case None => ResultWithIssues.forValue(None)
    }
  }

  def jsonToTag(json: Json): ResultWithIssues[String] = {
    json.asString match {
      case Some(string) => ResultWithIssues.forValue(string)
      case None => ResultWithIssues.forErrorMessage(
        s"String expected, but got $json."
      )
    }
  }

  def jsonToFunction(json: Json): ResultWithIssues[ScionFunction] = {
    json.asString match {
      case Some(name) => ScionNativeFunctions.get(name)
      case None => ResultWithIssues.forErrorMessage(
        s"String expected, but got $json."
      )
    }
  }

  def getTagOpt(json: Json): ResultWithIssues[Option[String]] = getOptionalChild(json, ScionDictionary.tagKey, jsonToTag)

  def parse(json: Json): ResultWithIssues[ScionGraph] = {
    val graphResultBox: ResultWithIssues.Box[ScionGraph] = ResultWithIssues.Box.forValue(ScionGraph.empty)
    for(jsonWithAnchor <- JsonCrawler.crawl(json)) {
      val json = jsonWithAnchor.json
      val path = jsonWithAnchor.path
      val tagResult = getOptionalChild(json, ScionDictionary.tagKey, jsonToTag)
      val functionResult = getOptionalChild(json, ScionDictionary.evalKey, jsonToFunction)
      val nodeOptionResult = tagResult.func2(functionResult) {
        case (Some(tag), Some(function)) => Some(ScionGraph.Node.create(json, path, tag, function))
        case (Some(tag), None) => Some(ScionGraph.Node.create(json, path, tag))
        case (None, Some(function)) => Some(ScionGraph.Node.create(json, path, function))
        case (None, None) => None
      }
      graphResultBox.insertOptional(nodeOptionResult)(_.plus(_))
    }
    graphResultBox.result
  }

}

