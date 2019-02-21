package scion.engine

import java.util.UUID

import better.files.File
import io.circe.parser.parse
import scion.engine.ScionFacade.{Command, CommandResult, RunCommand, RunResult}
import scion.util.ResultWithIssues

class ScionFacade {
  def execute(command: Command): ResultWithIssues[CommandResult[Command]] = {
    command match {
      case runCommand: RunCommand => run(runCommand)
    }
  }

  def run(runCommand: RunCommand): ResultWithIssues[RunResult] = {
    val files = runCommand.files
    val jsonResultByFile = files.map { file =>
      val contentResult = ResultWithIssues.fromTrying {
        file.contentAsString
      }
      val jsonResult =
        contentResult.flatMap(content => ResultWithIssues.fromThrowableEither(parse(content)))
      (file, jsonResult)
    }.toMap
    val jsonByFileResult = ResultWithIssues.consolidateMap(jsonResultByFile)
    val runResult = jsonByFileResult.flatMap { jsons =>
      val mainTag = runCommand.mainTag
      val engine = new ScionEngine()
      val engineResult = engine.run(mainTag, jsons)
      engineResult.map { engineResult =>
        RunResult(runCommand, engineResult)
      }
    }
    runResult
  }

}

object ScionFacade {

  sealed trait Command

  case class RunCommand(mainTag: String, files: Seq[File]) extends Command

  sealed trait CommandResult[+C <: Command] {
    def command: C
  }

  case class RunResult(uuid: UUID, command: RunCommand, engineResult: ScionEngine.Result)
    extends CommandResult[RunCommand]

  object RunResult {
    def apply(command: RunCommand, engineResult: ScionEngine.Result): RunResult = {
      val uuid = UUID.randomUUID()
      RunResult(uuid, command, engineResult)
    }
  }

}
