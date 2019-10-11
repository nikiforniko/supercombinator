case class NodeView(
  id: Int,
  label: String,
)

case class EdgeView(
  from: Int,
  to: Int,
)

case class GraphDiff(
  nodes: List[NodeView],
  edges: List[EdgeView],
)

case class Diff(
  command: String,
  add: GraphDiff,
  remove: GraphDiff,
  stack: List[NodeView],
)

case class Input(
  code: String,
)

case class DiffWithErr(
  diff: List[Diff],
  err: Option[String],
)
