package gmachine.machine

case class NodeView(
    id: Int,
    label: String,
    from: Option[Int],
    to: Option[Int]
)

case class EdgeView(
    from: Int,
    to: Int
)

case class GraphDiff(
    nodes: List[NodeView],
    edges: List[EdgeView]
)

case class Diff(
    command: String,
    add: GraphDiff,
    remove: GraphDiff,
    // TODO(a.eremeev) make Stack As Int
    stack: List[NodeView]
)

case class DiffWithErr(
    diff: List[Diff],
    err: Option[String],
    result: Option[String]
)
