package mathgraph.frontend
import mathgraph.util.Positioned

/** This object contains the definition of all the AST in mathgraph */
object Trees {
  abstract class Tree extends Positioned
  abstract class Expr extends Tree

  type Identifier = String

  // ----------------------------------------------------
  // Programs
  // ----------------------------------------------------

  case class Program(defs: Seq[Let], axioms: Seq[Expr]) extends Tree

  // ----------------------------------------------------
  // Definitions
  // ----------------------------------------------------

  /** Representation of let in(x, S) or let inc(A, B) = ... */
  case class Let(name: Identifier, vars: Seq[Identifier], body: Option[Expr])
      extends Tree

  // ----------------------------------------------------
  // Terms
  // ----------------------------------------------------

  /** Representation of true */
  case object True extends Expr

  /** Representation of false */
  case object False extends Expr

  /** Representation of lhs(arg1, ..., argN) or lhs if there are no args */
  case class Apply(id: Identifier, args: Seq[Expr]) extends Expr

  // ----------------------------------------------------
  // Formulas
  // ----------------------------------------------------

  /** Representation of a -> b */
  case class Implies(lhs: Expr, rhs: Expr) extends Expr

  /** Representation of forall */
  case class Forall(ids: Seq[Identifier], body: Expr) extends Expr

  /** TODO Representation of a = b */
  case class Equals(lhs: Expr, rhs: Expr) extends Expr

  // ----------------------------------------------------
  // Desugared expressions
  // ----------------------------------------------------

  /** Syntactic sugar for not */
  object Not {
    def apply(e: Expr): Expr = Implies(e, False)
    def unapply(e: Expr): Option[Expr] = e match {
      case Implies(e, False) => Some(e)
      case _                 => None
    }
  }

  /** Syntactic sugar for existential quantification */
  object Exists {
    def apply(ids: Seq[Identifier], body: Expr): Expr = Not(
      Forall(ids, Not(body))
    )
    def unapply(e: Expr): Option[(Seq[Identifier], Expr)] = e match {
      case Not(Forall(ids, Not(body))) => Some((ids, body))
      case _                           => None
    }
  }

  /** Syntactic sugar for and */
  object And {
    def apply(a: Expr, b: Expr): Expr =
      Implies(Implies(a, Implies(b, False)), False)
    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case Implies(Implies(a, Implies(b, False)), False) => Some((a, b))
      case _                                             => None
    }
  }

  /** Syntactic sugar for or */
  object Or {
    def apply(a: Expr, b: Expr): Expr = Not(And(Not(a), Not(b)))
    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case Not(And(Not(a), Not(b))) => Some((a, b))
      case _                        => None
    }
  }
}
