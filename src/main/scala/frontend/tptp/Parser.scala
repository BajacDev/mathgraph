package mathgraph.frontend.tptp
import mathgraph.util._
import mathgraph.frontend.Tokens.{Token => FrontendToken, _}
import mathgraph.frontend.Trees._
import scala.language.implicitConversions
import scallion._

object Parser extends Parsers with Pipeline[Iterator[FrontendToken], Program] {

  import Implicits._

  type Kind = TokenKind
  type Token = FrontendToken

  override def getKind(token: Token): TokenKind = kindOf(token)

  def kw(str: String): Syntax[Token] = elem(KwKind(str))
  def op(str: String): Syntax[Token] = elem(OperatorKind(str))
  def pred(str: String): Syntax[Token] = elem(PredicateKind(str))
  def delim(str: String): Syntax[Token] = elem(DelimKind(str))

  // implicit def delim(str: String): Syntax[Token] = elem(DelimKind(str)).skip
  implicit def skipped(str: String): Skip = str match {
    case " " => elem(SpaceKind).skip
    case _   => elem(DelimKind(str)).skip
  }

  lazy val tptp_file: Syntax[Program] = many(tptp_input).map { case inputs =>
    Program(Seq(), inputs.flatten)
  }

  lazy val tptp_input: Syntax[Seq[Expr]] = annotated_formula | include

  lazy val annotated_formula = (fof_annotated | cnf_annotated).map {
    case formula => Seq(formula)
  }

  lazy val fof_annotated: Syntax[Expr] = (kw(
    "fof"
  ) ~ "(" ~ name ~ "," ~ formula_role ~ "," ~ fof_formula ~ annotations ~ ")" ~ ".")
    .map {
      // case fof ~ name ~ ("conjecture", pos) ~ fof_formula ~ annotations => Implies(fof_formula, False).setPos(fof)
      case fof ~ name ~ formula_role ~ fof_formula ~ annotations =>
        formula_role match {
          case ("conjecture", pos) => Implies(fof_formula, False).setPos(fof)
          case _                   => fof_formula.setPos(fof)
        }
    }

  lazy val cnf_annotated = (kw(
    "cnf"
  ) ~ "(" ~ name ~ "," ~ formula_role ~ "," ~ cnf_formula ~ annotations ~ ")" ~ ".")
    .map { case cnf ~ name ~ formula_role ~ cnf_formula ~ annotations =>
      cnf_formula.setPos(cnf)
    }

  lazy val annotations = opt("," ~ source ~ optional_info).map { case _ =>
    None
  }

  lazy val formula_role = lower_word

  lazy val fof_formula: Syntax[Expr] = binary_formula | unitary_formula
  lazy val binary_formula: Syntax[Expr] = nonassoc_binary | assoc_binary

  lazy val nonassoc_binary: Syntax[Expr] =
    (unitary_formula ~ " " ~ binary_connective ~ " " ~ unitary_formula).map {
      case lhs ~ OperatorToken("=>") ~ rhs => Implies(lhs, rhs).setPos(lhs)
      case lhs ~ OperatorToken("<=") ~ rhs => Implies(rhs, lhs).setPos(lhs)
      case lhs ~ OperatorToken("<=>") ~ rhs =>
        And(Implies(lhs, rhs), Implies(rhs, lhs)).setPos(lhs)
      case _ ~ OperatorToken(op) ~ _ =>
        ??? //throw new java.lang.Error("Unsupported operator " + op)
    }

  lazy val binary_connective =
    op("<=>") | op("=>") | op("<=") | op("<~>") | op("~|") | op("~&")

  lazy val assoc_binary: Syntax[Expr] = or_formula | and_formula

  lazy val or_formula: Syntax[Expr] =
    (unitary_formula ~ " " ~ op("|") ~ " " ~ unitary_formula ~ " " ~ many(
      more_or_formula
    )).map { case lhs ~ _ ~ rhs ~ more =>
      (more.foldLeft(Or(lhs, rhs))((acc, next) => Or(acc, next))).setPos(lhs)
    }

  lazy val more_or_formula = op("|").skip ~ " " ~ unitary_formula

  lazy val and_formula: Syntax[Expr] =
    (unitary_formula ~ " " ~ op("&") ~ " " ~ unitary_formula ~ " " ~ many(
      more_or_formula
    )).map { case lhs ~ _ ~ rhs ~ more =>
      (more.foldLeft(And(lhs, rhs))((acc, next) => And(acc, next))).setPos(lhs)
    }

  lazy val more_and_formula = op("&").skip ~ " " ~ unitary_formula

  lazy val unitary_formula =
    quanitfied_formula | unary_formula | "(" ~ fof_formula ~ ")" | atomic_formula

  lazy val quanitfied_formula: Syntax[Expr] =
    (quantifier ~ "[" ~ variable_list ~ "]" ~ " " ~ ":" ~ " " ~ unitary_formula)
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

  lazy val unary_formula: Syntax[Expr] = (op("~") ~ " " ~ unitary_formula).map {
    case op ~ formula => Not(formula).setPos(op)
  }

  lazy val cnf_formula = "(" ~ disjunction ~ ")" | disjunction
  lazy val disjunction: Syntax[Expr] =
    (literal ~ " " ~ many(more_disjunction)).map { case lit ~ more =>
      (more.foldLeft(lit)((acc, next) => Or(acc, next))).setPos(lit)
    }
  lazy val more_disjunction = op("|").skip ~ " " ~ literal

  lazy val literal: Syntax[Expr] =
    (atomic_formula | negated_atomic_formula).map { case formula =>
      formula
    }
  lazy val negated_atomic_formula = (op("~") ~ atomic_formula).map {
    case op ~ formula => Not(formula).setPos(op)
  }

  lazy val atomic_formula = plain_atom | defined_atom | system_atom
  lazy val plain_atom = plain_term

  lazy val arguments = rep1sep(term, delim(",")).map { case args =>
    args
  }

  lazy val defined_atom: Syntax[Expr] = (yes | no | infix_defined_atom).map {
    case expr => expr
  }

  lazy val yes: Syntax[Expr] = pred("$true").map { case tk =>
    True.setPos(tk)
  }
  lazy val no: Syntax[Expr] = pred("$false").map { case tk =>
    False.setPos(tk)
  }

  lazy val infix_defined_atom: Syntax[Expr] =
    (term ~ " " ~ defined_infix_pred ~ " " ~ term).map {
      case lhs ~ PredicateToken("=") ~ rhs  => Equals(lhs, rhs).setPos(lhs)
      case lhs ~ PredicateToken("!=") ~ rhs => Not(Equals(lhs, rhs)).setPos(lhs)
      // defined_infix_pred only match pred("=") | pred("!=") but the compiler can't see that
      case op ~ variables ~ formula => ???
    }
  lazy val defined_infix_pred = pred("=") | pred("!=")

  lazy val system_atom = system_term

  lazy val term = function_term | term_variable
  lazy val term_variable: Syntax[Expr] = variable.map { case (id, pos) =>
    Apply(id, Seq()).setPos(pos)
  }
  lazy val function_term = plain_term | defined_term | system_term

  // val plain_term: Syntax[Expr] = (constant | functor ~ "(" ~ arguments ~ ")").map{
  //   case const => Apply(const._1, Seq()).setPos(const._2)
  //   case funct ~ args => Apply(funct._1, args).setPos(funct._2)
  // }
  // val constant = atomic_word
  // val functor = atomic_word
  lazy val plain_term: Syntax[Expr] =
    (atomic_word ~ opt("(" ~ arguments ~ ")")).map {
      case (word, pos) ~ None       => Apply(word, Seq()).setPos(pos)
      case (word, pos) ~ Some(args) => Apply(word, args).setPos(pos)
    }

  lazy val defined_term: Syntax[Expr] = (number | distinct_object).map {
    case (id, pos) =>
      Apply(id, Seq()).setPos(pos) // TODO Unclear what number really is
  }

  lazy val distinct_object: Syntax[(Identifier, Position)] =
    accept(DistinctObjectKind) { case tk @ DistinctObjectToken(distinct) =>
      (distinct, tk.pos)
    }

  // val system_term = system_constant | system_functor ~ "(" ~ arguments ~ ")"
  // val system_functor = atomic_system_word
  // val system_constant = atomic_system_word
  lazy val system_term: Syntax[Expr] =
    (atomic_system_word ~ opt("(" ~ arguments ~ ")")).map {
      case (word, pos) ~ None       => Apply(word, Seq()).setPos(pos)
      case (word, pos) ~ Some(args) => Apply(word, args).setPos(pos)
    }
  lazy val variable = upper_word

  lazy val source = general_term
  //dag_source> | <internal_source> | <external_source> | <unknown>

  /** %----Only a <dag_source> can be a <name>, i.e., derived formulae can be
    * %----identified by a <name> or an <inference_record>
    * <dag_source> :== <name> | <inference_record>
    * <inference_record> :== inference(<inference_rule>,<useful_info>,[<parent_list>])
    * <inference_rule> :== <atomic_word>
    * %----Examples are deduction | modus_tollens | modus_ponens | rewrite |
    * %                 resolution | paramodulation | factorization |
    * %                 cnf_conversion | cnf_refutation | ...
    * <parent_list>     :== <parent_info> | <parent_info>,<parent_list>
    * <parent_info>     :== <source><parent_details>
    * <parent_details>  :== :<atomic_word> | <null>
    * <internal_source> :== introduced(<intro_type><optional_info>)
    * <intro_type>      :== definition | axiom_of_choice | tautology
    * %----This should be used to record the symbol being defined, or the function
    * %----for the axiom of choice
    * <external_source> :== <file_source> | <theory> | <creator_source>
    * <file_source> :== file(<file_name><file_info>)
    * <file_info> :== ,<name> | <null>
    * <theory> :== theory(<theory_name><optional_info>)
    * <theory_name> :== equality | ac
    * %----More theory names may be added in the future. The <optional_info> is
    * %----used to store, e.g., which axioms of equality have been implicitly used,
    * %----e.g., theory(equality,[rst]). Standard format still to be decided.
    * <creator_source> :== creator(<creator_name><optional_info>)
    * <creator_name> :== <atomic_word>
    */

  lazy val optional_info = opt("," ~ useful_info)
  lazy val useful_info = general_term_list

  /** <useful_info> :== [] | [<info_items>]
    * <info_items> :== <info_item> | <info_item>,<info_items>
    * <info_item> :== <formula_item> | <inference_item> | <general_function>
    * %----Useful info for formula records
    * <formula_item> :== <description_item> | <iquote_item>
    * <description_item> :== description(<atomic_word>)
    * <iquote_item> :== iquote(<atomic_word>)
    * %----<iquote_item>s are used for recording exactly what the system output about
    * %----the inference step. In the future it is planned to encode this information
    * %----in standardized forms as <parent_details> in each <inference_record>.
    * %----Useful info for inference records
    * <inference_item> :== <inference_status> | <refutation>
    * <inference_status> :== status(<status_value>) | <inference_info>
    * %----These are the status values from the SZS ontology
    * <status_value> :== tau | tac | eqv | thm | sat | cax | noc | csa | cth |
    *                   ceq | unc | uns | sab | sam | sar | sap | csp | csr |
    *                   csm | csb
    * %----The most commonly used status values are:
    * %---- thm - Every model (and there are some) of the parent formulae is a
    * %----       model of the inferred formula. Regular logical consequences.
    * %---- cth - Every model (and there are some) of the parent formulae is a
    * %----       model of the negation of the inferred formula. Used for negation
    * %----       of conjectures in FOF to CNF conversion.
    * %---- sab - There is a bijection between the models (and there are some) of
    * %----       the parent formulae and models of the inferred formula. Used for
    * %----       Skolemization steps.
    * %----For the full hierarchy see the SZSOntology file distributed with the TPTP.
    * <inference_info> :== <inference_rule>(<atomic_word>,<general_list>)
    * <refutation> :== refutation(<file_source>)
    * %----Useful info for creators is just <general_function>
    */

  lazy val include: Syntax[Seq[Expr]] =
    (kw("include").skip ~ "(" ~ file_name ~ formula_selection ~ ")" ~ ".").map {
      case filename ~ None            => ???
      case filename ~ Some(selection) => ???
    }
  lazy val formula_selection = opt("," ~ "[" ~ name_list ~ "]")
  lazy val name_list = rep1sep(name, delim(","))

  lazy val general_term: Syntax[(Identifier, Position)] =
    general_list | general_data_term
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

  lazy val real = accept(RealKind) { case tk @ NumberToken(value) =>
    (value, tk.pos)
  }
  lazy val unsigned_integer = accept(UnsignedKind) {
    case tk @ NumberToken(value) => (value, tk.pos)
  }
  lazy val signed_integer = accept(SignedKind) { case tk @ NumberToken(value) =>
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

  protected def apply(tokens: Iterator[Token])(ctxt: Context): Program = {
    // val parser = Parser(program)
    //
    // parser(tokens) match {
    //   case Parsed(result, rest) => result
    //   case UnexpectedEnd(rest)  => ctxt.fatal("Unexpected end of input.")
    //   case UnexpectedToken(tk, rest) =>
    //     ctxt.fatal(
    //       "Unexpected token: " + tk + ", expected one of: " + rest.first
    //         .mkString(", "),
    //       tk
    //     )
    // }
    ???
  }
}
