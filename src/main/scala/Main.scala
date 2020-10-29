package mathgraph
import corelogic._
import frontend._
import io.AnsiColor._
import printer._
import repl.{CommandLexer, CommandParser, Repl}
import repl.Commands._
import scala.util.{Try, Success, Failure}
import scala.io.Source
import util._

object Main {

  def main(args: Array[String]): Unit = {

    val defaultExample = "example/test.txt"
    val pipeline = Lexer andThen Parser
    val ctx = new Context()

    val sourceFile = if (args.size >= 2) {
      args(1)
    } else {
      ctx.info(s"""Using default example: \"${defaultExample}\"""")
      defaultExample
    }

    val input = Source.fromFile(sourceFile).mkString

    try {
      val program = pipeline.run(input)(ctx)

      val initialGraph = LogicGraph.init
      val initialState =
        LogicState(initialGraph, Printer.init(initialGraph), None)

      val finalState = Repl(initialState)(ctx)

      ()
    } catch {
      case FatalError(_) => sys.exit(1)
    }
  }
}
