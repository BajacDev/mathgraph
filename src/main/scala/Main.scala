package mathgraph
import corelogic._
import frontend._
import io.AnsiColor._
import printer._
import repl.{CommandLexer, CommandParser}
import repl.Commands._
import scala.io.{Source, StdIn}
import scala.util.{Try, Success, Failure}
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
      val initialState = LogicState(initialGraph, Printer.init(initialGraph), None)

      val finalState = repl(initialState)(ctx)

      ()
    } catch {
      case FatalError(_) => sys.exit(1)
    }
  }

  def repl(currentState: LogicState)(implicit ctx: Context): LogicState = {

    val pipeline = CommandLexer andThen CommandParser

    System.out.print(s"${GREEN}>>> ${RESET}")

    val command = pipeline.run(StdIn.readLine())(ctx)

    command match {
      case Leave => currentState
      case _ => repl(command.apply(currentState))
    }
  }
}
