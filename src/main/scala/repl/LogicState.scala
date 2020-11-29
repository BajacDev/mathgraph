package mathgraph.repl

import mathgraph.corelogic._
import mathgraph.printer._
import mathgraph.solver._

case class LogicState(
    logicGraph: LogicGraph,
    printer: Printer,
    previousState: Option[LogicState],
    solver: Solver = new Solver()
)
