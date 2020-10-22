package mathgraph
import util._
import frontend._
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
      pipeline.run(input)(ctx)
    } catch {
      case FatalError(_) => sys.exit(1)
    }

    do {
      System.out.print(s"${GREEN}>>> ${RESET}")
    } while (process(ctx));
  }

  def process(ctx: Context): Boolean = {
    val pipeline = CommandLexer andThen CommandParser
    val command = pipeline.run(StdIn.readLine)(ctx)

    command.apply()

    command match {
      case Leave => {
        false
      }
      case _ => true
    }
  }
}
