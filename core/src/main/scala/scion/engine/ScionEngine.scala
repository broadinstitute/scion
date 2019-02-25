package scion.engine

import better.files.File
import io.circe.Json
import scion.engine.ScionExecuter.ExecutionResult
import scion.model.ScionGraph
import scion.util.ResultWithIssues

class ScionEngine {

  val parser: ScionParser = new ScionParser

  def run(mainTag: String, jsons: Map[File, Json]): ResultWithIssues[ScionEngine.Result] = {
    val graphsResultByKey = jsons.collect {
      case (file, json) => (file, parser.parse(file, json))
    }
    val graphsByKeyResult = ResultWithIssues.consolidateMap(graphsResultByKey)
    val executionResult = graphsByKeyResult.flatMap(ScionExecuter.execute(mainTag, _))
    graphsByKeyResult.func2(executionResult)(ScionEngine.Result(mainTag, _, _))
  }

}

object ScionEngine {
  case class Result(mainTag: String, graphs: Map[File, ScionGraph], executionResult: ExecutionResult)
}
