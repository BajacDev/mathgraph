package mathgraph.backend

import mathgraph.corelogic._
import mathgraph.frontend.BackendTrees._
import mathgraph.corelogic.ExprContainer._

object BackendContext {

  val impliesIdentifier = "->"
  val trueIdentifier = "true"
  val falseIdentifier = "false"
  val defIdentifier = "def"
  val forallIdentifier = "forall"

  def init = {
    val logicGraph = LogicGraph.init
    val e2s = Map(
      defIdentifier -> DefSymbol,
      falseIdentifier -> FalseSymbol,
      trueIdentifier -> TrueSymbol,
      impliesIdentifier -> ImplySymbol
    )
    BackendContext(logicGraph, e2s)
  }
}

case class BackendContext(
    logicGraph: LogicGraph,
    stringToExpr: Map[Name, Int]
)
