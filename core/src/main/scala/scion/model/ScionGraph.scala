package scion.model

import io.circe.Json
import scion.model.ScionGraph.{ExecutableNode, Node, TaggedExecutableNode, TaggedNode}
import scion.util.ResultWithIssues

case class ScionGraph(executableNodes: Set[ExecutableNode], taggedNodes: Set[TaggedNode]) {

}

object ScionGraph {

  case class TagToExecutableLink(tagged: TaggedNode, path: JsonPath, executable: ExecutableNode)

  def empty: ScionGraph = ScionGraph(Set.empty, Set.empty)

  def build(nodes: Set[Node]): ResultWithIssues[ScionGraph] = {
    val executableNodes = nodes.collect {
      case executableNode: ExecutableNode => executableNode
    }
    val taggedNodes = nodes.collect {
      case taggedNode: TaggedNode => taggedNode
    }
    val nodesByTag = taggedNodes.map(node => (node.tag, node)).toMap
    val tagToExecutableLinks = executableNodes.flatMap { executable =>
      taggedNodes.flatMap { tagged =>
        tagged.path.getFromThisTo(executable.path).map { path =>
          TagToExecutableLink(tagged, path, executable)
        }
      }
    }
    // TODO
    ResultWithIssues.forValue(ScionGraph(executableNodes, taggedNodes))
  }

  sealed trait Node {
    def json: Json

    def path: JsonPath

    def imports: Map[String, TagAncestry]

    def isTagged: Boolean

    def tagOpt: Option[String]

    def isExecutable: Boolean

    def functionOpt: Option[ScionFunction]
  }

  object Node {
    def create(json: Json, path: JsonPath, imports: Map[String, TagAncestry],
               tag: String): TaggedOnlyNode = TaggedOnlyNode(json, path, imports, tag)

    def create(json: Json, path: JsonPath, imports: Map[String, TagAncestry],
               function: ScionFunction): ExecutableOnlyNode =
      ExecutableOnlyNode(json, path, imports, function)

    def create(json: Json, path: JsonPath, imports: Map[String, TagAncestry], tag: String,
               function: ScionFunction): TaggedExecutableNode =
      TaggedExecutableNode(json, path, imports, tag, function)

    def create(json: Json, path: JsonPath, imports: Map[String, TagAncestry], tagOpt: Option[String],
               functionOpt: Option[ScionFunction]): Option[Node] = {
      (tagOpt, functionOpt) match {
        case (Some(tag), Some(function)) => Some(create(json, path, imports, tag, function))
        case (Some(tag), None) => Some(create(json, path, imports, tag))
        case (None, Some(function)) => Some(create(json, path, imports, function))
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

  final case class TaggedOnlyNode(json: Json, path: JsonPath, imports: Map[String, TagAncestry],
                                  tag: String) extends TaggedNode {
    override def isExecutable: Boolean = false

    override def functionOpt: Option[ScionFunction] = None
  }

  final case class ExecutableOnlyNode(json: Json, path: JsonPath, imports: Map[String, TagAncestry],
                                      function: ScionFunction) extends ExecutableNode {
    override def isTagged: Boolean = false

    override def tagOpt: Option[String] = None
  }

  final case class TaggedExecutableNode(json: Json, path: JsonPath, imports: Map[String, TagAncestry],
                                        tag: String, function: ScionFunction)
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
