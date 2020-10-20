
package mathgraph.util
import mathgraph.util

/**
 * This represents a phase of the compiler or of the solver. Pipelines can be composed together.
 */
trait Pipeline[-S, +T] { self =>
  /** This method should apply the pipeline to an input of type S, giving a result of type T */
  protected def apply(s: S)(ctxt: Context): T

  /** This can be used to chain two pipelines together, stopping if errors occur in between */
  def andThen[U](that: Pipeline[T, U]): Pipeline[S, U] = new Pipeline[S, U] {
    protected def apply(s: S)(ctxt: Context): U = {
      that(self.run(s)(ctxt))(ctxt)
    }
  }

  /** This method runs the given pipeline and stops the program if errors occured */
  def run(s: S)(ctxt: Context): T = {
    val t = apply(s)(ctxt)
    ctxt.terminateIfErrors()
    t
  }
}
