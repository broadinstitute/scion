package scion.model

import io.circe.Json
import scion.model.ScionGraph.{ExecutableNode, Node, TaggedExecutableNode, TaggedNode}
import scion.util.ResultWithIssues

case class ScionGraph(executableNodes: Set[ExecutableNode], taggedNodes: Set[TaggedNode]) {

  def plus(node: Node): ScionGraph = {
    node match {
      case taggedExecutableNode: TaggedExecutableNode =>
        copy(
          executableNodes = executableNodes + taggedExecutableNode,
          taggedNodes = taggedNodes + taggedExecutableNode
        )
      case taggedNode: TaggedNode => copy(taggedNodes = taggedNodes + taggedNode)
      case executableNode: ExecutableNode => copy(executableNodes = executableNodes + executableNode)
    }
  }

}

object ScionGraph {

  def empty: ScionGraph = ScionGraph(Set.empty, Set.empty)

  sealed trait Node {
    def json: Json

    def path: JsonPath

    def isTagged: Boolean

    def tagOpt: Option[String]

    def isExecutable: Boolean

    def functionOpt: Option[ScionFunction]
  }

  object Node {
    def create(json: Json, path: JsonPath, tag: String): TaggedOnlyNode = TaggedOnlyNode(json, path, tag)

    def create(json: Json, path: JsonPath, function: ScionFunction): ExecutableOnlyNode =
      ExecutableOnlyNode(json, path, function)

    def create(json: Json, path: JsonPath, tag: String, function: ScionFunction): TaggedExecutableNode =
      TaggedExecutableNode(json, path, tag, function)

    def create(json: Json, path: JsonPath, tagOpt: Option[String], functionOpt: Option[ScionFunction]): Option[Node] = {
      (tagOpt, functionOpt) match {
        case (Some(tag), Some(function)) => Some(create(json, path, tag, function))
        case (Some(tag), None) => Some(create(json, path, tag))
        case (None, Some(function)) => Some(create(json, path, function))
        case (None, None) => None
      }
    }
  }

  sealed trait TaggedNode extends Node {
    override def isTagged: Boolean = true

    override def tagOpt: Option[String] = Some(tag)

    def tag: String
  }

  sealed trait ExecutableNode extends Node {
    override def isExecutable: Boolean = true

    override def functionOpt: Option[ScionFunction] = Some(function)

    def function: ScionFunction
  }

  final case class TaggedOnlyNode(json: Json, path: JsonPath, tag: String) extends TaggedNode {
    override def isExecutable: Boolean = false

    override def functionOpt: Option[ScionFunction] = None
  }

  final case class ExecutableOnlyNode(json: Json, path: JsonPath, function: ScionFunction) extends ExecutableNode {
    override def isTagged: Boolean = false

    override def tagOpt: Option[String] = None
  }

  final case class TaggedExecutableNode(json: Json, path: JsonPath, tag: String, function: ScionFunction)
    extends TaggedNode with ExecutableNode

  final case class TagAncestry(tag: String, path: JsonPath)

  object TagAncestry {
    def fromString(string: String): ResultWithIssues[TagAncestry] = {
      val parts = string.split(":")
      if(parts.size < 3) {
        ResultWithIssues.forErrorMessage(s"Malformed tag reference '$string': Needs to contain at least two ':'")
      } else if(parts(0) != "tag") {
        ResultWithIssues.forErrorMessage(s"Malformed tag reference '$string': Needs to start with 'tag:'")
      } else {
        val tag = parts(1)
        val pathString = parts.drop(2).mkString(":")
        val regexMatchingDot = "[.]"
        val path = JsonPath(pathString.split(regexMatchingDot).map(JsonPath.KeyElement).toSeq)
        ResultWithIssues.forValue(TagAncestry(tag, path))
      }
    }
  }

}
