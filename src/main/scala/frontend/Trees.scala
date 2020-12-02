package mathgraph.frontend
import mathgraph.util.{Positioned, Position}

/** This object contains the definition of all the AST in mathgraph */
trait Trees {
  abstract class Tree extends Positioned
  abstract class Expr extends Tree
  abstract class Definition(name: Name, params: Seq[Name], body: Option[Expr]) extends Tree

  /** This represents the names that are used in the program */
  type Name

  // ----------------------------------------------------
  // Programs
  // ----------------------------------------------------

  case class Program(defs: Seq[Definition], axioms: Seq[Expr]) extends Tree

  // ----------------------------------------------------
  // Definitions
  // ----------------------------------------------------

  /** Representation of let in(x, S) or let inc(A, B) = ... */
  case class Let(name: Name, params: Seq[Name], body: Option[Expr])
      extends Definition(name, params, body)

  // ----------------------------------------------------
  // Terms
  // ----------------------------------------------------

  /** Representation of true */
  case object True extends Expr

  /** Representation of false */
  case object False extends Expr

  /** Representation of lhs(arg1, ..., argN) or lhs if there are no args */
  case class Apply(name: Name, args: Seq[Expr]) extends Expr

  // ----------------------------------------------------
  // Formulas
  // ----------------------------------------------------

  /** Representation of a -> b */
  case class Implies(lhs: Expr, rhs: Expr) extends Expr

  /** Representation of forall */
  case class Forall(names: Seq[Name], body: Expr) extends Expr

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
    def apply(a: Expr, b: Expr): Expr = Not(Implies(a, Not(b)))
    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case Not(Implies(a, Not(b))) => Some((a, b))
      case _ => None
    }
  }

  /** Syntactic sugar for or */
  object Or {
    def apply(a: Expr, b: Expr): Expr = Implies(Not(a), b)
    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case Implies(Not(a), b) => Some((a, b))
      case _ => None
    }
  }
}

/** Represents the trees where the names are not yet unique */
object NominalTrees extends Trees {
  type Name = String
}

/** Represents the trees where the names are unique */
object MGLTrees extends Trees {
  type Name = Identifier
}

/** Represents trees where operators haven't been parsed yet and names aren't unique */
object OpTrees extends Trees {
  type Name = String

  /** Representation of an operator definition let(<associativity>, <precedence>) a op b [:= <rhs>]; */
  case class OpLet(
      assoc: String,
      prec: String,
      lhs: String,
      op: String,
      rhs: String,
      body: Option[Expr]
  ) extends Definition(op, Seq(lhs, rhs), body)

  /** Representation of a sequence of operator applications x1 op1 x2 op2 ... xn */
  case class OpSequence(first: Expr, opsAndExprs: Seq[(String, Position, Expr)])
      extends Expr
}
