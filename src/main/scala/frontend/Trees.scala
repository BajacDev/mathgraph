package mathgraph.frontend
import mathgraph.util.{Positioned, Position}

/** This object contains the definition of all the AST in mathgraph */
trait Trees {
  trait Tree extends Positioned
  trait Expr extends Tree

  /** This represents the names that are used in the program */
  type Name = String

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
  case class Implies private (lhs: Expr, rhs: Expr) extends Expr
  object Implies {
    def apply(lhs: Expr, rhs: Expr): Expr = (lhs, rhs) match {
      case (True, e)       => e
      case (False, _)      => True
      case (_, True)       => True
      case (Not(e), False) => e
      case (lhs, rhs)      => new Implies(lhs, rhs)
    }
  }

  /** Representation of forall */
  case class Forall private (names: Seq[Name], body: Expr) extends Expr
  object Forall {
    def apply(names: Seq[Name], body: Expr): Expr =
      if (names.isEmpty) body
      else
        body match {
          case True                    => True
          case False                   => False
          case Forall(moreNames, body) => new Forall(names ++ moreNames, body)
          case _                       => new Forall(names, body)
        }
  }

  /** TODO Representation of a = b */
  case class Equals(lhs: Expr, rhs: Expr) extends Expr

  // ----------------------------------------------------
  // Desugared expressions
  // ----------------------------------------------------

  /** Syntactic sugar for not */
  object Not {
    def apply(e: Expr): Expr = e match {
      case True   => False
      case False  => True
      case Not(e) => e
      case _      => Implies(e, False)
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
      case _                       => None
    }
  }

  /** Syntactic sugar for or */
  object Or {
    def apply(a: Expr, b: Expr): Expr = Implies(Not(a), b)
    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case Implies(Not(a), b) => Some((a, b))
      case _                  => None
    }
  }
}

/** Trees of the mathgraph language and internal representation */
trait MGLTrees extends Trees {

  /** Represents a definition in a program. It can either be a symbol definition,
    *      with no body, or a rewriting rule, with a body
    */
  trait Definition extends Tree {
    def name: Name
    def params: Seq[Name]
    def body: Option[Expr]
  }

  /** A program consists of a sequence of definitions and a set of axioms to prove unsatisfiable */
  case class Program(defs: Seq[Definition], axioms: Seq[Expr]) extends Tree

  /** Representation of let in(x, S) or let inc(A, B) = ... */
  case class Let(name: Name, params: Seq[Name], body: Option[Expr])
      extends Definition
}

/** Represents the trees where the names are unique, ready to be passed to the backend */
object BackendTrees extends MGLTrees

/** Represents trees where operators haven't been parsed yet and names aren't unique */
object OpTrees extends MGLTrees {

  /** Representation of an operator definition let(<associativity>, <precedence>) a op b [:= <rhs>]; */
  case class OpLet(
      assoc: String,
      prec: String,
      lhs: Name,
      op: Name,
      rhs: Name,
      body: Option[Expr]
  ) extends Definition {
    def name = op
    def params = Seq(lhs, rhs)
  }

  /** Representation of a sequence of operator applications x1 op1 x2 op2 ... xn */
  case class OpSequence(first: Expr, opsAndExprs: Seq[(Name, Position, Expr)])
      extends Expr
}

/** Represents the TPTP trees where the names are not yet unique and there are some unresolved includes */
object TPTPTrees extends Trees {

  /** Represents an include statement */
  case class Include(filename: String, formulas: Seq[String]) extends Tree

  /** Represents a TPTP program */
  case class Program(includes: Seq[Include], formulas: Seq[Annotated])
      extends Tree

  /** Represents an annotated formula */
  trait Annotated extends Tree {
    def name: String
    def expr: Expr
  }

  /** Represents an axiom */
  case class Axiom(name: String, expr: Expr) extends Annotated

  /** Represents a conjecture */
  case class Conjecture(name: String, expr: Expr) extends Annotated
}
