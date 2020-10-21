package mathgraph
import util._
import frontend._
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val pipeline = Lexer andThen Parser
    val input = Source.fromFile("input.txt").mkString

    try {
      pipeline.run(input)(new Context())
    } catch {
      case FatalError(_) => sys.exit(1)
    }
  }
}
