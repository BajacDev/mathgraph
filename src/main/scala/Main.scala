package mathgraph
import corelogic._
import frontend.mgl.{Lexer, Parser}
import frontend.tptp.{Translator}
import backend._
import repl._
import util._

object Main {

  def main(args: Array[String]): Unit = {

    val ctxt = new Context()

    val defaultFile = "example/test.txt"
    val sourceFile = if (args.isEmpty) {
      ctxt.info(s"Using default input file: $defaultFile")
      defaultFile
    } else args(0)

    val pipeline =
      frontend(sourceFile) andThen Simplifier andThen ForallToLets andThen ProgToLogicState

    try {
      val logicState: LogicState = pipeline.run(FileSource(sourceFile))(ctxt)
      Repl(logicState)(ctxt)
    } catch {
      case FatalError(_) => sys.exit(1)
    }
  }

  def frontend(sourceFile: String) = {
    if (sourceFile.contains(".p")) Translator
    else Lexer andThen Parser
  }
}
