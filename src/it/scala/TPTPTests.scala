package mathgraph.solver

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.concurrent._
import org.scalatest.time._
import mathgraph.solver._
import mathgraph.corelogic._
import mathgraph.frontend.TPTPFrontend
import mathgraph.backend._
import mathgraph.util._
import mathgraph.repl.LogicState
import scala.concurrent.{ExecutionContext, Future}
import java.util.concurrent.Executors
import java.io.File

class TPTPTests extends AnyFunSuite with TimeLimits {

  val NoFilter: String => Boolean = _ => true

  def listFiles(root: File, filter: String => Boolean = NoFilter): Seq[File] = {
    if (!root.exists || !root.isDirectory)
      return Seq()

    val (subDirs, files) = root.listFiles.partition(_.isDirectory)
    files.filter(f => filter(f.getName)) ++ subDirs.flatMap(listFiles(_, filter))
  }

  val testRoot = new File("resources/tptp")
  val filter: String => Boolean = _.endsWith(".p")
  val pipeline = TPTPFrontend andThen Simplifier andThen ForallToLets andThen ProgToLogicState
  val timeout = 5
  implicit val context = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  implicit val signaler: Signaler = ThreadSignaler // kills the main thread in case of a timeout

  def testFile(file: File): Unit = {
    val filename = file.getPath
    val testName = testRoot.toPath.relativize(file.toPath).toString
    
    test(testName) {
      try {
        val logicState = pipeline.run(FileSource(filename))(new Context)
        val lg = logicState.logicGraph
        var newLg: LogicGraph = null

        failAfter(Span(timeout, Seconds)) {
          val thread = new Thread {
            override def run() = {
              newLg = Solver.saturation(lg)
            }
          }

          thread.start()
          thread.join()
        }

        assert(newLg.isAbsurd)
      } catch {
        case FatalError(_) => fail(s"There was an error in file '$filename'")
      }
    }
  }

  // Test all the files in the given directory, with the given filter
  for (file <- listFiles(testRoot, filter)) testFile(file)

}
