package scion.app

import better.files.File
import io.circe.{Json, ParsingFailure}
import io.circe.parser.parse

object ScionApp {

  trait Command

  case class RunCommand(mainTag: String, files: Seq[File]) extends Command

  def parseArgs(args: Seq[String]): Either[String, Command] = {
    if(args.size < 1) {
      Left("Need at least one argument.")
    } else {
      val commandString = args.head
      commandString match {
        case "run" =>
          if(args.size < 3) {
            Left("'run' command needs at main tag and at least one file")
          } else {
            val mainTag = args(1)
            val files = args.drop(2).map(File(_))
            Right(RunCommand(mainTag, files))
          }
      }
    }
  }

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
