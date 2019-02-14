package scion.engine

import java.io.IOException

import better.files.File
import io.circe.{Json, ParsingFailure}
import scion.engine.ScionFacade.{Command, CommandResult, RunCommand, RunEngineResult, RunIoFailure, RunParseFailure, RunResult, RunValidationFailure}
import io.circe.parser.parse
import scion.engine.ScionValidator.Issue

class ScionFacade {
  def execute(command: Command): CommandResult[Command] = {
    command match {
      case runCommand: RunCommand => run(runCommand)
    }
  }

  def run(runCommand: RunCommand): RunResult = {
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
        val validator = new ScionValidator()
        val mainTag = runCommand.mainTag
        val issues = validator.validate(mainTag, jsons)
        if (issues.nonEmpty) {
          RunValidationFailure(runCommand, issues)
        } else {
          val engine = new ScionEngine()
          val engineResult = engine.run(mainTag, jsons)
          RunEngineResult(runCommand, engineResult)
        }
      }
    }

  }

}

object ScionFacade {

  sealed trait Command

  case class RunCommand(mainTag: String, files: Seq[File]) extends Command

  sealed trait CommandResult[+C <: Command] {
    def command: C

    def wasSuccess: Boolean

    def message: String
  }

  trait RunResult extends CommandResult[RunCommand] {
    override def command: RunCommand

    override def wasSuccess: Boolean

    def message: String
  }


  trait RunFailure extends RunResult {
    override def command: RunCommand

    override def wasSuccess: Boolean = false
  }

  case class RunIoFailure(command: RunCommand, ioExceptions: Map[File, IOException]) extends RunFailure {
    override def message: String = {
      ioExceptions.collect({
        case (file, ioException) => s"Problem reading $file: ${ioException.getMessage}."
      }).mkString("; ")
    }
  }

  case class RunParseFailure(command: RunCommand, parsingFailures: Map[File, ParsingFailure]) extends RunFailure {
    override def message: String = {
      parsingFailures.collect({
        case (file, parsingFailure) => s"Problem reading $file: ${parsingFailure.getMessage}."
      }).mkString("; ")
    }
  }

  case class RunValidationFailure(command: RunCommand, issues: Seq[Issue]) extends RunFailure {
    override def message: String = s"There were ${issues.size} issue(s): \n" + issues.map(_.message).mkString("\n")
  }

  case class RunEngineResult(command: RunCommand, engineResult: ScionEngine.Result) extends RunResult {
    override def message: String = engineResult.message

    override def wasSuccess: Boolean = engineResult.wasSuccess
  }

}
