package scion.app

import better.files.File
import io.circe.{Json, ParsingFailure}
import io.circe.parser.parse

object ScionApp {

  def printParsingFailure(parsingFailure: ParsingFailure): Unit = {
    println(parsingFailure)
  }

  def processJson(json: Json): Unit = {
    println("Got some nice JSON!")
    // TODO
  }

  def main(args: Array[String]): Unit = {
    for(inFileName <- args) {
      val inFile = File(inFileName)
      if(inFile.exists) {
        val content = inFile.contentAsString
        parse(content) match {
          case Left(parsingFailure) => printParsingFailure(parsingFailure)
          case Right(json) => processJson(json)
        }
      } else {
        println(s"File $inFileName does not exist.")
      }
    }
  }

}
