package mathgraph
import util._
import frontend._

object Main {
	def main(args: Array[String]): Unit = {
		val pipeline = Parser
		
    try {
      pipeline.run("input.txt")(new Context())
    } catch {
      case FatalError(_) => sys.exit(1)
    }
	}
}
