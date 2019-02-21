package scion.engine

import better.files.File
import io.circe.Json
import scion.model.ScionGraph
import scion.util.ResultWithIssues

class ScionEngine {

  val parser: ScionParser = new ScionParser

  def run(mainTag: String, jsons: Map[File, Json]): ResultWithIssues[ScionEngine.Result] = {
    val graphsResultByKey = jsons.mapValues(parser.parse).view.force
    val graphsByKeyResult = ResultWithIssues.consolidateMap(graphsResultByKey)
    graphsByKeyResult.map(ScionEngine.Result(mainTag, _))
  }

}

object ScionEngine {
  case class Result(mainTag: String, graphs: Map[File, ScionGraph])
}
