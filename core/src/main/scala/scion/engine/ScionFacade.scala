package scion.engine

import java.io.IOException
import java.util.UUID

import better.files.File
import io.circe.parser.parse
import io.circe.{Json, ParsingFailure}
import scion.engine.ScionFacade.{Command, CommandResult, RunCommand, RunEngineResult, RunIoFailure, RunParseFailure, RunResult}
import scion.util.ResultWithIssues

class ScionFacade {
  def execute(command: Command): CommandResult[Command] = {
    command match {
      case runCommand: RunCommand => run(runCommand)
    }
  }

  def run(runCommand: RunCommand): ResultWithIssues[RunResult] = {
    val files = runCommand.files
    val contentEithers: Map[File, Either[IOException, String]] = files.map { file =>
      try {
        (file, Right(file.contentAsString))
      } catch {
        case ioException: IOException => (file, Left(ioException))
      }
    }.toMap
    val ioExceptions: Map[File, IOException] = contentEithers.collect {
      case (file, Left(iOException)) => (file, iOException)
    }
    if (ioExceptions.nonEmpty) {
      RunIoFailure(runCommand, ioExceptions)
    } else {
      val contents = contentEithers.collect {
        case (file, Right(content)) => (file, content)
      }
      val jsonEithers: Map[File, Either[ParsingFailure, Json]] = contents.collect {
        case (file, content) => (file, parse(content))
      }
      val parsingFailures: Map[File, ParsingFailure] = jsonEithers.collect {
        case (file, Left(parsingFailure)) => (file, parsingFailure)
      }
      if (parsingFailures.nonEmpty) {
        RunParseFailure(runCommand, parsingFailures)
      } else {
        val jsons: Map[File, Json] = jsonEithers.collect {
          case (file, Right(json)) => (file, json)
        }
        val mainTag = runCommand.mainTag
        val engine = new ScionEngine()
        val engineResult = engine.run(mainTag, jsons)
        RunEngineResult(runCommand, engineResult)
      }
    }

  }

}

object ScionFacade {

  sealed trait Command

  case class RunCommand(mainTag: String, files: Seq[File]) extends Command

  sealed trait CommandResult[+C <: Command] {
    def command: C
  }

  case class RunResult(uuid: UUID, command: RunCommand) extends CommandResult[RunCommand]

  object RunResult {
    def apply(command: RunCommand): RunResult = {
      val uuid = UUID.randomUUID()
      RunResult(uuid, command)
    }
  }

}
