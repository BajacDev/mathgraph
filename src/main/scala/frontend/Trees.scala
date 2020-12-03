package mathgraph.frontend
import mathgraph.util.{Positioned, Position}

/** This object contains the definition of all the AST in mathgraph */
trait Trees {
  trait Tree extends Positioned
  trait Expr extends Tree
  trait Definition extends Tree {
    def name: Name
    def params: Seq[Name]
    def body: Option[Expr]
  }

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
  case class Let(name: Name, params: Seq[Name], body: Option[Expr]) extends Definition

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
    def apply(e: Expr): Expr = e match {
      case Not(e) => e
      case _ => Implies(e, False)
    }
    def unapply(e: Expr): Option[Expr] = e match {
      case Implies(e, False) => Some(e)
      case _                 => None
    }
  }

  /** Syntactic sugar for existential quantification */
  object Exists {
    def apply(ids: Seq[Name], body: Expr): Expr = Not(
      Forall(ids, Not(body))
    )
    def unapply(e: Expr): Option[(Seq[Name], Expr)] = e match {
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

/** Represents the TPTP trees where the names are not yet unique and there are some unresolved includes */
object TPTPTrees extends Trees {
  type Name = String

  /** Represents an include statement */
  case class Include(filename: String, formulas: Seq[String]) extends Tree

  /** Represents a TPTP program */
  case class TPTPProgram(includes: Seq[Include], formulas: Seq[Annotated]) extends Tree

  /** Represents an annotated formula */
  abstract class Annotated(name: String, expr: Expr) extends Tree

  /** Represents an axiom */
  case class Axiom(name: String, expr: Expr) extends Annotated(name, expr)

  /** Represents a conjecture */
  case class Conjecture(name: String, expr: Expr) extends Annotated(name, expr)
}

/** Represents the trees where the names are unique */
object BackendTrees extends Trees {
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
  ) extends Definition {
    def name = op
    def params = Seq(lhs, rhs)
  }

  /** Representation of a sequence of operator applications x1 op1 x2 op2 ... xn */
  case class OpSequence(first: Expr, opsAndExprs: Seq[(String, Position, Expr)])
      extends Expr
}
