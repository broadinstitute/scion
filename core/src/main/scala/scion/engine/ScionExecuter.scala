package scion.engine

import better.files.File
import scion.model.{ScionGraph, ScionType, ScionValue}
import scion.model.ScionGraph.ExecutableNode
import scion.util.ResultWithIssues

/**
  * scion
  * Created by oruebenacker on 2/24/19.
  */
object ScionExecuter {

  def execute(mainTag: String, graphs: Map[File, ScionGraph]): ResultWithIssues[ExecutionResult] = {
    var nodesToExecute = graphs.values.toSet.flatMap(_.tagToExecutables(mainTag))
    var returnValues: Map[ExecutableNode, ScionValue] = Map.empty
    var noErrorYet = true
    var issues: Seq[ResultWithIssues.Issue] = Seq.empty
    while(nodesToExecute.nonEmpty && noErrorYet) {
      var newNodesToExecute: Set[ExecutableNode] = Set.empty
      val nextNode = nodesToExecute.head
      val function = nextNode.function
      val arguments = function.arguments
      val jsonValues = nextNode.json.asObject.get.toMap
      val imports = nextNode.imports
      var readyToExecute = true
      var argumentValues: Map[String, ScionValue] = Map.empty
      for((key, scionType) <- arguments) {
        val valueOpt = if(jsonValues.contains(key)) {
          val jsonArgument = jsonValues(key)
          if(scionType == ScionType.file && jsonArgument.isString) {
            Some(ScionValue.FileValue(File(jsonArgument.asString.get)))
          } else if(scionType == ScionType.string && jsonArgument.isString) {
            Some(ScionValue.StringValue(jsonArgument.asString.get))
          } else {
            None
          }
        } else if (imports.contains(key)) {
          val tagAncestry = imports(key)
          val dependentNode = graphs(nextNode.file).tagAncestryToExecutable(tagAncestry)
          returnValues.get(dependentNode) match {
            case Some(value) => Some(value)
            case None =>
              newNodesToExecute += dependentNode
              readyToExecute = false
              None
          }
        } else {
          issues :+= ResultWithIssues.SimpleError(s"No value for $key found for function ${function.name}.")
          None
        }
        valueOpt match {
          case Some(value) => argumentValues += (key, value)
          case None => ()
        }
      }



      nodesToExecute = nodesToExecute ++ newNodesToExecute
    }
    ??? // TODO
  }

  case class ExecutionResult(message: String)
}
