package mathgraph
import corelogic._
import frontend.mgl.{Lexer, Parser, OpsRewrite}
import frontend.tptp.{TPTPFrontend}
import backend._
import repl._
import util._

object Main {

  def main(args: Array[String]): Unit = {
    val ctxt = new Context
    val defaultFile = "example/test.txt"
    val sourceFile = if (args.isEmpty) {
      ctxt.info(s"Using default input file: $defaultFile")
      defaultFile
    } else s"example/${args(0)}.txt"

    val pipeline =
      frontend(
        sourceFile
      ) andThen Simplifier andThen ForallToLets andThen ProgToLogicState

    try {
      val logicState: LogicState = pipeline.run(FileSource(sourceFile))(ctxt)
      Repl.run(logicState)
    } catch {
      case FatalError(_) => sys.exit(1)
    }
  }

  def frontend(sourceFile: String) = {
    if (sourceFile.contains(".p")) TPTPFrontend
    else Lexer andThen Parser andThen OpsRewrite
  }
}
