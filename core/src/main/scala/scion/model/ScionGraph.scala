package scion.model

import better.files.File
import io.circe.Json
import scion.model.ScionGraph._
import scion.util.ResultWithIssues

case class ScionGraph(executableNodes: Set[ExecutableNode],
                      taggedNodes: Set[TaggedNode],
                      tagToNode: Map[String, TaggedNode],
                      tagAncestryToExecutable: Map[TagAncestry, ExecutableNode],
                      tagToExecutables: Map[String, Set[ExecutableNode]]
                     ) {

}

object ScionGraph {

  case class TagToExecutableLink(tagged: TaggedNode, path: JsonPath, executable: ExecutableNode)

  def build(nodes: Set[Node]): ResultWithIssues[ScionGraph] = {
    val executableNodes = nodes.collect {
      case executableNode: ExecutableNode => executableNode
    }
    val taggedNodes = nodes.collect {
      case taggedNode: TaggedNode => taggedNode
    }
    val tagToNode = taggedNodes.map(node => (node.tag, node)).toMap
    val tagToExecutableLinks = executableNodes.flatMap { executable =>
      taggedNodes.flatMap { tagged =>
        tagged.path.getFromThisTo(executable.path).map { path =>
          TagToExecutableLink(tagged, path, executable)
        }
      }
    }
    println(tagToExecutableLinks)
    val tagAncestryToExecutable =
      tagToExecutableLinks.map(link => (TagAncestry(link.tagged.tag, link.path), link.executable)).toMap
    val tagToExecutables =
      tagToExecutableLinks.groupBy(_.tagged.tag).mapValues(_.map(_.executable)).view.force
    ResultWithIssues.forValue(
      ScionGraph(executableNodes, taggedNodes, tagToNode, tagAncestryToExecutable, tagToExecutables)
    )
  }

  sealed trait Node {
    def file: File

    def json: Json

    def path: JsonPath

    def imports: Map[String, TagAncestry]

    def isTagged: Boolean

    def tagOpt: Option[String]

    def isExecutable: Boolean

    def functionOpt: Option[ScionFunctionSignature]
  }

  object Node {
    def create(file: File, json: Json, path: JsonPath, imports: Map[String, TagAncestry],
               tag: String): TaggedOnlyNode = TaggedOnlyNode(file, json, path, imports, tag)

    def create(file: File, json: Json, path: JsonPath, imports: Map[String, TagAncestry],
               function: ScionFunctionSignature): ExecutableOnlyNode =
      ExecutableOnlyNode(file, json, path, imports, function)

    def create(file: File, json: Json, path: JsonPath, imports: Map[String, TagAncestry], tag: String,
               function: ScionFunctionSignature): TaggedExecutableNode =
      TaggedExecutableNode(file, json, path, imports, tag, function)

    def create(file: File, json: Json, path: JsonPath, imports: Map[String, TagAncestry], tagOpt: Option[String],
               functionOpt: Option[ScionFunctionSignature]): Option[Node] = {
      (tagOpt, functionOpt) match {
        case (Some(tag), Some(function)) => Some(create(file, json, path, imports, tag, function))
        case (Some(tag), None) => Some(create(file, json, path, imports, tag))
        case (None, Some(function)) => Some(create(file, json, path, imports, function))
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

    override def functionOpt: Option[ScionFunctionSignature] = Some(function)

    def function: ScionFunctionSignature
  }

  final case class TaggedOnlyNode(file: File, json: Json, path: JsonPath, imports: Map[String, TagAncestry],
                                  tag: String) extends TaggedNode {
    override def isExecutable: Boolean = false

    override def functionOpt: Option[ScionFunctionSignature] = None
  }

  final case class ExecutableOnlyNode(file: File, json: Json, path: JsonPath, imports: Map[String, TagAncestry],
                                      function: ScionFunctionSignature) extends ExecutableNode {
    override def isTagged: Boolean = false

    override def tagOpt: Option[String] = None
  }

  final case class TaggedExecutableNode(file: File, json: Json, path: JsonPath, imports: Map[String, TagAncestry],
                                        tag: String, function: ScionFunctionSignature)
    extends TaggedNode with ExecutableNode

  final case class TagAncestry(tag: String, path: JsonPath)

  object TagAncestry {
    def fromString(string: String): ResultWithIssues[TagAncestry] = {
      val parts = string.split(":")
      if (parts.size < 3) {
        ResultWithIssues.forErrorMessage(s"Malformed tag reference '$string': Needs to contain at least two ':'")
      } else if (parts(0) != "tag") {
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
