package scion.model

import io.circe.Json
import scion.model.ScionGraph.{ExecutableNode, Node, TaggedExecutableNode, TaggedNode}

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

    def isTagged: Boolean

    def tagOpt: Option[String]

    def isExecutable: Boolean

    def functionOpt: Option[ScionFunction]
  }

  object Node {
    def create(json: Json, tag: String): TaggedOnlyNode = TaggedOnlyNode(json, tag)

    def create(json: Json, function: ScionFunction): ExecutableOnlyNode = ExecutableOnlyNode(json, function)

    def create(json: Json, tag: String, function: ScionFunction): TaggedExecutableNode =
      TaggedExecutableNode(json, tag, function)

    def create(json: Json, tagOpt: Option[String], functionOpt: Option[ScionFunction]): Option[Node] = {
      (tagOpt, functionOpt) match {
        case (Some(tag), Some(function)) => Some(create(json, tag, function))
        case (Some(tag), None) => Some(create(json, tag))
        case (None, Some(function)) => Some(create(json, function))
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

  final case class TaggedOnlyNode(json: Json, tag: String) extends TaggedNode {
    override def isExecutable: Boolean = false

    override def functionOpt: Option[ScionFunction] = None
  }

  final case class ExecutableOnlyNode(json: Json, function: ScionFunction) extends ExecutableNode {
    override def isTagged: Boolean = false

    override def tagOpt: Option[String] = None
  }

  final case class TaggedExecutableNode(json: Json, tag: String, function: ScionFunction)
    extends TaggedNode with ExecutableNode

}
