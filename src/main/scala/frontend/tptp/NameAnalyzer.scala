package mathgraph.frontend.tptp
import mathgraph.util._
import mathgraph.frontend.{NominalTrees => In, BackendTrees => Out}

object NameAnalyzer extends Pipeline[In.Program, (Out.Program, SymbolTable)] {
  protected def apply(program: In.Program)(ctxt: Context): Out.Program = {
    
  }
}
