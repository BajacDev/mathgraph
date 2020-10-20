
package mathgraph.frontend
import mathgraph.util._
import mathgraph.frontend.Trees._

/** A parser takes as input the name of a file and outputs a program */
class Parser extends Pipeline[String, Program] {
  protected def apply(file: String)(ctxt: Context): Program = {
    Program(Seq(), Seq())
  }
}
