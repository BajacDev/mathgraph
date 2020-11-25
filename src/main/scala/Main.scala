package mathgraph
import corelogic._
import frontend._
import backend._
import repl._
import util._

object Main {

  def main(args: Array[String]): Unit = {
    val pipeline =
      Lexer andThen Parser andThen Simplifier andThen ForallToLets andThen ProgToLogicState
    val ctxt = new Context

    val defaultFile = "example/test.txt"
    val sourceFile = if (args.isEmpty) {
      ctxt.info(s"Using default input file: $defaultFile")
      defaultFile
    } else args(0)

    try {
      val logicState = pipeline.run(FileSource(sourceFile))(ctxt)
      Repl.run(logicState)
    } catch {
      case FatalError(_) => sys.exit(1)
    }
  }
}
