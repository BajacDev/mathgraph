
package mathgraph.util

/**
 * This represents a phase of the compiler or of the solver. Pipelines can be composed together.
 */
trait Pipeline[-S, +T] { self =>
    def run(s: S)(ctxt: Context): T

    def andThen(that: Pipeline[T, U]): Pipeline[S, U] = new Pipeline {
        def run(s: S)(ctxt: Context): U = {
            that.run(self.run(s)(ctxt))(ctxt)
        }
    }
}
