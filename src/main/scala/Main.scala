package mathgraph
import util._
import frontend._
import corelogic._
import repl.{CommandLexer, CommandParser}
import repl.Commands._
import scala.io.{Source, StdIn}
import scala.util.{Try, Success, Failure}
import io.AnsiColor._

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

      val finalGraph = repl(initialGraph, Nil)(ctx)

      ()
    } catch {
      case FatalError(_) => sys.exit(1)
    }
  }

  def repl(current: LogicGraph, previous: List[LogicGraph])(implicit ctx: Context): (LogicGraph, List[LogicGraph]) = {

    val pipeline = CommandLexer andThen CommandParser

    System.out.print(s"${GREEN}>>> ${RESET}")

    val command = pipeline.run(StdIn.readLine())(ctx)

    command match {
      case Leave => (current, previous)
      case _ => {
        val (newCurrent, newPrevious) = command.apply(current, previous)
        repl(newCurrent, newPrevious)
      }
    }
  }
}
