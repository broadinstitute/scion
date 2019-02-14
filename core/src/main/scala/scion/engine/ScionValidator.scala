package scion.engine

import io.circe.Json
import scion.engine.ScionValidator.Issue

class ScionValidator {

  def validate[K](mainTag: String, jsons: Map[K, Json]): Seq[Issue] = {
    ???
  }

}

object ScionValidator {
  trait Issue{
    def message: String
  }
}
