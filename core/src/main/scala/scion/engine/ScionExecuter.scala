package scion.engine

import better.files.File
import scion.model.ScionGraph.ExecutableNode
import scion.model.{ScionGraph, ScionType, ScionValue}
import scion.util.ResultWithIssues

object ScionExecuter {

  def executablesReport(executables: Iterable[ExecutableNode]): String =
    if (executables.nonEmpty) {
      executables.toSeq.map(_.function.signature.name).mkString(", ")
    } else {
      "[none]"
    }

  def executablesReport(todo: Iterable[ExecutableNode], done: Iterable[ExecutableNode]): String =
    "TODO: " + executablesReport(todo) + "; DONE: " + executablesReport(done)

  def execute(mainTag: String, graphs: Map[File, ScionGraph]): ResultWithIssues[ExecutionResult] = {
    var nodesToExecute = graphs.values.toSet[ScionGraph].flatMap(_.tagToExecutables(mainTag))
    var nodesExecuted: Set[ExecutableNode] = Set.empty
    var returnValues: Map[ExecutableNode, ScionValue] = Map.empty
    var noErrorYet = true
    var issues: Seq[ResultWithIssues.Issue] = Seq.empty
    var counter = 10
    while (nodesToExecute.nonEmpty && noErrorYet && counter > 0) {
      var newNodesToExecute: Set[ExecutableNode] = Set.empty
      var newNodesExecuted: Set[ExecutableNode] = Set.empty
      counter -= 1
      println(executablesReport(nodesToExecute, nodesExecuted))
      for (currentNode <- nodesToExecute) {
        val function = currentNode.function
        val arguments = function.signature.arguments
        val jsonValues = currentNode.json.asObject.get.toMap
        val imports = currentNode.imports
        var readyToExecute = true
        var argumentValues: Map[String, ScionValue] = Map.empty
        for ((key, scionType) <- arguments) {
          val valueOpt = if (jsonValues.contains(key)) {
            val jsonArgument = jsonValues(key)
            if (scionType == ScionType.file && jsonArgument.isString) {
              Some(ScionValue.FileValue(File(jsonArgument.asString.get)))
            } else if (scionType == ScionType.string && jsonArgument.isString) {
              Some(ScionValue.StringValue(jsonArgument.asString.get))
            } else {
              None
            }
          } else if (imports.contains(key)) {
            val tagAncestry = imports(key)
            val dependentNode = graphs(currentNode.file).tagAncestryToExecutable(tagAncestry)
            returnValues.get(dependentNode) match {
              case Some(value) => Some(value)
              case None =>
                newNodesToExecute += dependentNode
                readyToExecute = false
                None
            }
          } else {
            issues :+= ResultWithIssues.SimpleError(s"No value for $key found for function ${function.signature.name}.")
            None
          }
          valueOpt match {
            case Some(value) => argumentValues += (key -> value)
            case None => ()
          }
        }
        if (readyToExecute) {
          val returnValue = currentNode.function(argumentValues)
          returnValues += (currentNode -> returnValue)
          newNodesExecuted += currentNode
        }
      }
      nodesExecuted ++= newNodesExecuted
      nodesToExecute = nodesToExecute ++ newNodesToExecute -- nodesExecuted
    }
    println(executablesReport(nodesToExecute, nodesExecuted))
    if (noErrorYet) {
      ResultWithIssues.forValue(ExecutionResult("Seems to have worked!")).withIssues(issues)
    } else {
      ResultWithIssues.Failure(issues)
    }
  }

  case class ExecutionResult(message: String)

}
