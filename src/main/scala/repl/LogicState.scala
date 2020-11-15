package mathgraph.repl

import mathgraph.corelogic._
import mathgraph.printer._

object LogicState {
  def init: LogicState = {
    val logicGraph = LogicGraph.init
    LogicState(logicGraph, Printer.init(logicGraph), None)
  }
}

case class LogicState(
    logicGraph: LogicGraph,
    printer: Printer,
    previousState: Option[LogicState]
)
