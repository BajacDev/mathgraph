package mathgraph.solver

import org.scalatest.funsuite.AnyFunSuite
import mathgraph.solver._
import mathgraph.corelogic._
import mathgraph.frontend.mgl._
import mathgraph.backend._
import io.AnsiColor._
import mathgraph.printer._
import mathgraph.repl.LogicState
import mathgraph.repl.Commands._
import scala.util.{Try, Success, Failure}
import scala.io.Source
import mathgraph.util._
import mathgraph.util.Pipe._

class LogicGraphTest extends AnyFunSuite {

  test("saturation works") {

    val pipeline =
      Lexer andThen Parser andThen OpsRewrite andThen Simplifier andThen ForallToLets andThen ProgToLogicState
    val ctx = new Context()

    val sourceFile = "example/solvertest/test1.txt"

    try {
      val logicState: LogicState = pipeline.run(FileSource(sourceFile))(ctx)
      val lg = logicState.logicGraph
      val solver = logicState.solver
      solver.saturation(lg)
      assert(lg.isAbsurd)

      ()
    } catch {
      case FatalError(_) => assert(false)
    }

  }
}
