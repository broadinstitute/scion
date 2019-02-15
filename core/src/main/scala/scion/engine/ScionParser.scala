package scion.engine

import io.circe.Json
import scion.engine.ScionParser.{Failure, Issue, Result, Success}
import scion.model.ScionGraph

class ScionParser {

  def parse(json: Json): Result = {
    var graph = ScionGraph.empty
    var issues = Seq.empty[Issue]
    var foundNoErrorYet = true

    if(foundNoErrorYet) {
      Success(graph, issues)
    } else {
      Failure(issues)
    }
  }

}

object ScionParser {
  case class Issue(isError: Boolean, message: String)

  sealed trait Result {
    def issues: Seq[Issue]
    def errors: Seq[Issue] = issues.filter(_.isError)
    def warnings: Seq[Issue] = issues.filterNot(_.isError)
  }

  final case class Success(graph: ScionGraph, issues: Seq[Issue]) extends Result
  final case class Failure(issues: Seq[Issue]) extends Result
}
