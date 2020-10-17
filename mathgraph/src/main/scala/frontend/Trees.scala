
package mathgraph.frontend

trait Trees {
    abstract class Tree extends Positioned
    abstract class Expr extends Tree
    abstract class Def extends Tree

    type Identifier = String

    // ----------------------------------------------------
    // Programs
    // ----------------------------------------------------
    
    case class Program(defs: Seq[Def], axioms: Seq[Expr]) extends Tree



    // ----------------------------------------------------
    // Definitions
    // ----------------------------------------------------

    /** Representation of let in(x, S) or let inc(A, B) = ... */
    case class Let(name: Identifier, vars: Seq[Identifier], body: Option[Expr]) extends Def



    // ----------------------------------------------------
    // Expressions
    // ----------------------------------------------------

    /** Representation of x */
    case class Symbol(id: Identifier) extends Expr

    /** Representation of a -> b */
    case class Implies(lhs: Expr, rhs: Expr) extends Expr

    /** Representation of true */
    case object True extends Expr

    /** Representation of false */
    case object False extends Expr

    /** Representation of lhs(arg1, ..., argN) */
    case class Apply(lhs: Expr, args: Seq[Expr]) extends Expr

    /** Representation of forall */
    case class Forall(var: Identifier, body: Expr) extends Expr
}
