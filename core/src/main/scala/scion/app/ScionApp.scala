package scion.app

import better.files.File
import scion.engine.ScionFacade
import scion.engine.ScionFacade.{Command, RunCommand}

object ScionApp {

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
        case _ =>
          Left("The only valid command is 'run'.")
      }
    }
  }

  val scionFacade: ScionFacade = new ScionFacade

  def main(args: Array[String]): Unit = {
    parseArgs(args) match {
      case Left(message) => println(message)
      case Right(command) =>
        val commandResult = scionFacade.execute(command)
        println(commandResult.message)
    }
  }

}
