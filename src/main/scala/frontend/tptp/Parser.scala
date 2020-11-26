package mathgraph.frontend.tptp
import mathgraph.util._
import mathgraph.frontend.Tokens.{Token => FrontendToken, _}
import mathgraph.frontend.Trees._
import scala.language.implicitConversions
import scallion._

object Parser
    extends Parsers
    with Pipeline[Iterator[FrontendToken], Seq[Tree]] {

  import Implicits._

  type Kind = TokenKind
  type Token = FrontendToken

  case class TPTP_Include(filename: String, formulas: Seq[(String, Position)])
      extends Tree {
    def unapply(tree: Tree): Option[(String, Seq[(String, Position)])] =
      tree match {
        case TPTP_Include(filename, formulas) => Some((filename, formulas))
        case _                                => None
      }
  }

  override def getKind(token: Token): TokenKind = kindOf(token)

  def kw(str: String): Syntax[Token] = elem(KwKind(str))
  def op(str: String): Syntax[Token] = elem(OperatorKind(str))
  def pred(str: String): Syntax[Token] = elem(PredicateKind(str))
  def delim(str: String): Syntax[Token] = elem(DelimKind(str))

  val eof: Syntax[Token] = elem(EOFKind)

  implicit def skipped(str: String): Skip = str match {
    case " " => elem(SpaceKind).skip
    case _   => elem(DelimKind(str)).skip
  }

  lazy val tptp_file: Syntax[Seq[Tree]] = (many(tptp_input) ~ eof.skip).map {
    case inputs =>
      inputs
  }

  lazy val tptp_input: Syntax[Tree] = annotated_formula | include

  lazy val annotated_formula = (fof_annotated | cnf_annotated).map {
    case formula => formula
  }

  lazy val fof_annotated: Syntax[Tree] = (kw(
    "fof"
  ) ~ "(" ~ name ~ "," ~ formula_role ~ "," ~ fof_formula ~ annotations ~ ")" ~ ".")
    .map {
      case fof ~ name ~ formula_role ~ fof_formula ~ annotations => {

        val body = formula_role match {
          case ("conjecture", pos) => Implies(fof_formula, False).setPos(fof)
          case _                   => fof_formula.setPos(fof)
        }

        Let(name._1, Seq(), Some(body)).setPos(fof)
      }
    }

  lazy val cnf_annotated: Syntax[Tree] = (kw(
    "cnf"
  ) ~ "(" ~ name ~ "," ~ formula_role ~ "," ~ cnf_formula ~ annotations ~ ")" ~ ".")
    .map { case cnf ~ name ~ formula_role ~ cnf_formula ~ annotations =>

      val body = formula_role match {
        case ("conjecture", pos) => Implies(cnf_formula, False).setPos(cnf)
        case _                   => cnf_formula.setPos(cnf)
      }

      Let(name._1, Seq(), Some(body)).setPos(cnf)
    }

  lazy val annotations = opt("," ~ source ~ optional_info).map { case _ =>
    None
  }

  lazy val formula_role = lower_word

  lazy val fof_formula: Syntax[Expr] =
    (unitary_formula ~ opt(assoc_binary_op | non_assoc_binary_op)).map {
      case unitary ~ None => unitary
      case lhs ~ Some((OperatorToken("=>"), Seq(rhs))) =>
        Implies(lhs, rhs).setPos(lhs)
      case lhs ~ Some((OperatorToken("<="), Seq(rhs))) =>
        Implies(rhs, lhs).setPos(lhs)
      case lhs ~ Some((OperatorToken("<=>"), Seq(rhs))) =>
        And(Implies(lhs, rhs), Implies(rhs, lhs)).setPos(lhs)

      case lhs ~ Some((OperatorToken("&"), rhs +: more)) =>
        (
          more.foldLeft(And(lhs, rhs))((acc, next) => And(acc, next))
        ).setPos(lhs)
      case lhs ~ Some((OperatorToken("|"), rhs +: more)) =>
        (more.foldLeft(Or(lhs, rhs))((acc, next) => Or(acc, next))).setPos(lhs)
      case _ ~ _ => UnknownExpr
    }

  lazy val assoc_binary_op: Syntax[(Token, Seq[Expr])] =
    or_formula | and_formula

  lazy val or_formula: Syntax[(Token, Seq[Expr])] = many1(more_or_formula).map {
    case formulas => (OperatorToken("|"), formulas)
  }
  lazy val more_or_formula = op("|").skip ~ unitary_formula

  lazy val and_formula: Syntax[(Token, Seq[Expr])] =
    many1(more_and_formula).map { case formulas =>
      (OperatorToken("&"), formulas)
    }
  lazy val more_and_formula = op("&").skip ~ unitary_formula

  lazy val binary_connective =
    op("<=>") | op("=>") | op("<=") | op("<~>") | op("~|") | op("~&")

  lazy val non_assoc_binary_op: Syntax[(Token, Seq[Expr])] =
    (binary_connective ~ unitary_formula).map { case connective ~ formula =>
      (connective, Seq(formula))
    }

  lazy val unitary_formula: Syntax[Expr] =
    recursive(
      quanitfied_formula | unary_formula | "(" ~ fof_formula ~ ")" | atomic_formula
    )

  lazy val quanitfied_formula: Syntax[Expr] =
    (quantifier ~ "[" ~ variable_list ~ "]" ~ ":" ~ unitary_formula)
      .map {
        case (op @ OperatorToken("!")) ~ variables ~ formula =>
          Forall(variables, formula).setPos(op)
        case (op @ OperatorToken("?")) ~ variables ~ formula =>
          Exists(variables, formula).setPos(op)
        // quantifier only match op("!") and op("?") but the compiler can't see that
        case op ~ variables ~ formula => ???
      }

  lazy val quantifier = op("!") | op("?")

  lazy val variable_list = rep1sep(variable, delim(",")).map { case variables =>
    variables.map(_._1)
  }

  lazy val unary_formula: Syntax[Expr] = (op("~") ~ unitary_formula).map {
    case op ~ formula => Not(formula).setPos(op)
  }

  lazy val cnf_formula = "(" ~ disjunction ~ ")" | disjunction
  lazy val disjunction: Syntax[Expr] =
    (literal ~ many(more_disjunction)).map { case lit ~ more =>
      (more.foldLeft(lit)((acc, next) => Or(acc, next))).setPos(lit)
    }
  lazy val more_disjunction = op("|").skip ~ literal

  lazy val literal: Syntax[Expr] =
    (atomic_formula | negated_atomic_formula).map { case formula =>
      formula
    }
  lazy val negated_atomic_formula = (op("~") ~ atomic_formula).map {
    case op ~ formula => Not(formula).setPos(op)
  }

  lazy val atomic_formula = yes | no | plain_system_defined_atom

  lazy val arguments = rep1sep(term, delim(",")).map { case args =>
    args
  }

  lazy val yes: Syntax[Expr] = pred("$true").map { case tk =>
    True.setPos(tk)
  }
  lazy val no: Syntax[Expr] = pred("$false").map { case tk =>
    False.setPos(tk)
  }

  lazy val plain_system_defined_atom: Syntax[Expr] =
    (defined_term_variable_infix | plain_system_or_defined_infix)

  lazy val defined_infix_pred = pred("=") | pred("!=")
  lazy val defined_term_variable_infix =
    ((defined_term | term_variable) ~ defined_infix_pred ~ term).map {
      case lhs ~ PredicateToken("=") ~ rhs  => Equals(lhs, rhs).setPos(lhs)
      case lhs ~ PredicateToken("!=") ~ rhs => Not(Equals(lhs, rhs)).setPos(lhs)
      // defined_infix_pred only match pred("=") | pred("!=") but the compiler can't see that
      case lhs ~ op ~ rhs => ???
    }

  lazy val plain_system_or_defined_infix =
    (plain_or_system_term ~ opt(defined_infix_pred ~ term)).map {
      case t ~ None                              => t
      case lhs ~ Some(PredicateToken("=") ~ rhs) => Equals(lhs, rhs).setPos(lhs)
      case lhs ~ Some(PredicateToken("!=") ~ rhs) =>
        Not(Equals(lhs, rhs)).setPos(lhs)
      // defined_infix_pred only match pred("=") | pred("!=") but the compiler can't see that
      case lhs ~ Some(op ~ rhs) => ???
    }

  lazy val term = recursive(function_term | term_variable)

  lazy val term_variable: Syntax[Expr] = variable.map { case (id, pos) =>
    Apply(id, Seq()).setPos(pos)
  }

  lazy val function_term = defined_term | plain_or_system_term

  lazy val plain_or_system_term: Syntax[Expr] = recursive(
    (lower_word | single_quoted | dollar_dollar_word) ~ opt(
      "(" ~ arguments ~ ")"
    )
  ).map {
    case (word, pos) ~ None       => Apply(word, Seq()).setPos(pos)
    case (word, pos) ~ Some(args) => Apply(word, args).setPos(pos)
  }

  lazy val defined_term: Syntax[Expr] = (number | distinct_object).map {
    case (id, pos) =>
      Apply(id, Seq()).setPos(pos)
  }

  lazy val distinct_object: Syntax[(Identifier, Position)] =
    accept(DistinctObjectKind) { case tk @ DistinctObjectToken(distinct) =>
      (distinct, tk.pos)
    }

  lazy val system_term: Syntax[Expr] =
    (atomic_system_word ~ opt("(" ~ arguments ~ ")")).map {
      case (word, pos) ~ None       => Apply(word, Seq()).setPos(pos)
      case (word, pos) ~ Some(args) => Apply(word, args).setPos(pos)
    }
  lazy val variable = upper_word

  lazy val source = general_term

  lazy val optional_info = opt("," ~ useful_info)
  lazy val useful_info = general_term_list

  lazy val include: Syntax[Tree] =
    (kw("include").skip ~ "(" ~ file_name ~ formula_selection ~ ")" ~ ".").map {
      case (filename, pos) ~ None => TPTP_Include(filename, Seq()).setPos(pos)
      case (filename, pos) ~ Some(selection) =>
        TPTP_Include(filename, selection).setPos(pos)
    }
  lazy val formula_selection = opt("," ~ "[" ~ name_list ~ "]")
  lazy val name_list = rep1sep(name, delim(","))

  lazy val general_term: Syntax[(Identifier, Position)] =
    recursive(general_list | general_data_term)

  lazy val general_data: Syntax[(Identifier, Position)] =
    atomic_word_with_arguments | number | distinct_object

  lazy val general_data_term: Syntax[(Identifier, Position)] =
    (general_data ~ opt(":" ~ general_term)).map {
      case (data, pos) ~ None            => (data, pos)
      case (data, pos) ~ Some((term, _)) => (data + ":" + term, pos)
    }

  lazy val atomic_word_with_arguments: Syntax[(Identifier, Position)] =
    (atomic_word ~ opt("(" ~ general_arguments ~ ")")).map {
      case (word, pos) ~ None => (word, pos)
      case (word, pos) ~ Some(args) =>
        (word + "(" + args.mkString(",") + ")", pos)
    }

  lazy val general_list: Syntax[(Identifier, Position)] =
    (delim("[") ~ opt(general_term_list) ~ delim("]")).map {
      case delim ~ None ~ _ => ("[]", delim.pos)
      case delim ~ Some(terms) ~ _ =>
        ("[" + terms.mkString(",") + "]", delim.pos)
    }

  lazy val general_arguments: Syntax[Seq[Identifier]] =
    rep1sep(general_term, delim(",")).map { case args =>
      args.map(_._1)
    }
  lazy val general_term_list: Syntax[Seq[Identifier]] =
    rep1sep(general_term, delim(",")).map { case args =>
      args.map(_._1)
    }

  lazy val name = atomic_word | unsigned_integer
  lazy val atomic_word: Syntax[(Identifier, Position)] =
    lower_word | single_quoted

  lazy val atomic_system_word = lower_word | dollar_dollar_word
  lazy val number = real | signed_integer | unsigned_integer

  lazy val file_name = atomic_word

  lazy val real = accept(RealKind) { case tk @ RealToken(value) =>
    (value, tk.pos)
  }
  lazy val unsigned_integer = accept(UnsignedKind) {
    case tk @ UnsignedToken(value) => (value, tk.pos)
  }
  lazy val signed_integer = accept(SignedKind) { case tk @ SignedToken(value) =>
    (value, tk.pos)
  }

  lazy val dollar_dollar_word: Syntax[(Identifier, Position)] =
    accept(DollarWordKind) { case tk @ DollarWordToken(word) =>
      (word, tk.pos)
    }

  lazy val lower_word: Syntax[(Identifier, Position)] = accept(LowerWordKind) {
    case tk @ WordToken(word) => (word, tk.pos)
  }

  lazy val upper_word: Syntax[(Identifier, Position)] = accept(UpperWordKind) {
    case tk @ WordToken(word) => (word, tk.pos)
  }

  lazy val single_quoted: Syntax[(Identifier, Position)] =
    accept(SingleQuotedKind) { case tk @ SingleQuotedToken(quoted) =>
      (quoted, tk.pos)
    }

  // Ensures the grammar is in LL(1), otherwise prints some counterexamples
  lazy val checkLL1: Boolean = {
    if (tptp_file.isLL1) {
      true
    } else {
      debug(tptp_file)
      false
    }
  }

  protected def apply(tokens: Iterator[Token])(ctxt: Context): Seq[Tree] = {

    val parser = Parser(tptp_file)

    parser(tokens) match {
      case Parsed(result, rest) => result
      case UnexpectedEnd(rest)  => ctxt.fatal("Unexpected end of input.")
      case UnexpectedToken(tk, rest) =>
        ctxt.fatal(
          "Unexpected token: " + tk + ", expected one of: " + rest.first
            .mkString(", "),
          tk
        )
    }
  }
}
