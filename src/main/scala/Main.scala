package mathgraph
import corelogic._
import frontend._
import backend._
import repl.{CommandLexer, CommandParser, Repl, LogicState}
import util._

object Main {

  def main(args: Array[String]): Unit = {
    val pipeline =
      Lexer andThen Parser andThen Simplifier andThen ForallToLets andThen ProgToLogicState
    val ctxt = new Context()

    val defaultFile = "example/test.txt"
    val sourceFile = if (args.isEmpty) {
      ctxt.info(s"Using default input file: $defaultFile")
      defaultFile
    } else args(0)

    try {
      val logicState: LogicState = pipeline.run(FileSource(sourceFile))(ctxt)
      Repl(logicState)(ctxt)
    } catch {
      case FatalError(_) => sys.exit(1)
    }
  }
}
