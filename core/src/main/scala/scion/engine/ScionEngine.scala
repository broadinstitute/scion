package scion.engine

import io.circe.Json
import scion.engine.ScionEngine.Result

class ScionEngine {

  def run[K](mainTag: String, jsons: Map[K, Json]): Result = {
    ??? // TODO
  }

}

object ScionEngine {
  sealed trait Result {
    def wasSuccess: Boolean
    def message: String
  }

  case class Success(message: String) extends Result {
    override def wasSuccess: Boolean = true
  }
}
