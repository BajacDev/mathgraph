package mathgraph.repl

import mathgraph.corelogic._
import mathgraph.printer._

case class LogicState(
    logicGraph: LogicGraph,
    printer: Printer,
    previousState: Option[LogicState]
)
