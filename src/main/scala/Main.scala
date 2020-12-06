package mathgraph
import corelogic._
import frontend._
import backend._
import repl._
import util._

object Main {

  def main(args: Array[String]): Unit = {
    val ctxt = new Context
    val defaultFile = "resources/mgl/test.txt"
    val sourceFile = if (args.isEmpty) {
      ctxt.info(s"Using default input file: $defaultFile")
      defaultFile
    } else args(0)

    val pipeline = {
      if (sourceFile.endsWith(".p")) TPTPFrontend else MGLFrontend
    } andThen Simplifier andThen ForallToLets andThen ProgToLogicState

    try {
      val logicState: LogicState = pipeline.run(FileSource(sourceFile))(ctxt)
      Repl.run(logicState)
    } catch {
      case FatalError(_) => sys.exit(1)
    }
  }
}
