package mathgraph
import corelogic._
import frontend._
import backend._
import io.AnsiColor._
import printer._
import repl.{CommandLexer, CommandParser, Repl, LogicState}
import repl.Commands._
import scala.util.{Try, Success, Failure}
import scala.io.Source
import util._

object Main {

  def main(args: Array[String]): Unit = {

    val defaultExample = s"example/${args(0)}.txt"
    val pipeline =
      Lexer andThen Parser andThen Simplifier andThen ForallToLets andThen ProgToLogicState
    val ctx = new Context()

    val sourceFile = if (args.size >= 2) {
      args(1)
    } else {
      ctx.info(s"""Using default example: \"${defaultExample}\"""")
      defaultExample
    }

    val input = Source.fromFile(sourceFile).mkString

    try {
      val logicState: LogicState = pipeline.run(input)(ctx)
      val finalState = Repl(logicState)(ctx)

      ()
    } catch {
      case FatalError(_) => sys.exit(1)
    }
  }
}
