package scion.engine

import io.circe.Json
import scion.util.ResultWithIssues

class ScionEngine {

  def run[K](mainTag: String, jsons: Map[K, Json]): ResultWithIssues[Unit] = {
    ??? // TODO
  }

}

object ScionEngine {
}
