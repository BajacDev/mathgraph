package mathgraph.frontend
import mathgraph.util._
import mathgraph.frontend.{ OpTrees => In, MGLTrees => Out }

object OpsRewrite extends Pipeline[In.Program, Out.Program] {
  protected def apply(program: In.Program)(ctxt: Context): Out.Program = {
    program.asInstanceOf[Out.Program]
  }
}
