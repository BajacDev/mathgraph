package mathgraph.backend

import mathgraph.corelogic._
import mathgraph.frontend.Trees._

object BackendContext {

  val impliesIdentifier = "->"
  val trueIdentifier = "true"
  val falseIdentifier = "false"
  val defIdentifier = "def"
  val forallIdentifier = "forall"

  def init = {
    val logicGraph = LogicGraph.init
    val e2s = Map(
      defIdentifier -> logicGraph.defPos,
      falseIdentifier -> logicGraph.falsePos,
      trueIdentifier -> logicGraph.truePos,
      impliesIdentifier -> logicGraph.implyPos
    )
    BackendContext(logicGraph, e2s)
  }
}

case class BackendContext(
    logicGraph: LogicGraph,
    stringToExpr: Map[Identifier, Int]
)
