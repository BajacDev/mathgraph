package mathgraph.frontend.tptp

import mathgraph.util._
import mathgraph.frontend.Tokens.{Token => FrontendToken, _}
import mathgraph.frontend.Trees._
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

  val tptp_file = many(tptp_input)

  val tptp_input = annotated_formula | include

  val annotated_formula = fof_annotated | cnf_annotated

  val fof_annotated = (kw(
    "fof"
  ).skip ~ "(" ~ name ~ "," ~ formula_role ~ "," ~ fof_formula ~ annotations ~ ")" ~ ".")
    .map { case name ~ formula_role ~ fof_formula ~ annotations =>
      ???
    }

  val cnf_annotated = (kw(
    "cnf"
  ).skip ~ "(" ~ name ~ "," ~ formula_role ~ "," ~ cnf_formula ~ annotations ~ ")" ~ ".")
    .map { case name ~ formula_role ~ fof_formula ~ annotations =>
      ???
    }

  val annotations = opt("," ~ source ~ optional_info)

  val formula_role = lower_word

  val fof_formula = binary_formula | unitary_formula
  val binary_formula = nonassoc_binary | assoc_binary

  val nonassoc_binary =
    unitary_formula ~ " " ~ binary_connective ~ " " ~ unitary_formula
  val binary_connective =
    op("<=>") | op("=>") | op("<=") | op("<~>") | op("~|") | op("~&")

  val assoc_binary = or_formula | and_formula
  val or_formula =
    unitary_formula ~ " " ~ op("|") ~ " " ~ unitary_formula ~ ??? ~ many(
      more_or_formula
    )
  val more_or_formula = op("|") ~ " " ~ unitary_formula

  val and_formula =
    unitary_formula ~ " " ~ op("&") ~ " " ~ unitary_formula ~ ??? ~ many(
      more_or_formula
    )
  val more_and_formula = op("&") ~ " " ~ unitary_formula

  val unitary_formula =
    quanitfied_formula | unary_formula | "(" ~ fof_formula ~ ")" | atomic_formula
  val quanitfied_formula =
    quantifier ~ "[" ~ variable_list ~ "]" ~ " " ~ ":" ~ " " ~ unitary_formula
  val quantifier = op("!") | op("?")

  val variable_list = rep1sep(variable, delim(","))

  val unary_formula = op("~") ~ " " ~ unitary_formula

  val cnf_formula = "(" ~ disjunction ~ ")" | disjunction
  val disjunction = literal ~ " " ~ many(more_disjunction)
  val more_disjunction = op("|") ~ " " ~ literal
  val literal = atomic_formula | op("~") ~ atomic_formula

  val atomic_formula = plain_atom | defined_atom | system_atom
  val plain_atom = plain_term

  val arguments = rep1sep(term, delim(","))
  val defined_atom = pred("$true") | pred("$false") |
    term ~ " " ~ defined_infix_pred ~ " " ~ term

  val defined_infix_pred = pred("=") | pred("!=")

  val system_atom = system_term

  val term = function_term | variable
  val function_term = plain_term | defined_term | system_term
  val plain_term = constant | functor ~ "(" ~ arguments ~ ")"
  val constant = atomic_word
  val functor = atomic_word
  val defined_term = number | distinct_object

  val distinct_object = accept(DistinctObjectKind)

  val system_term = system_constant | system_functor ~ "(" ~ arguments ~ ")"
  val system_functor = atomic_system_word
  val system_constant = atomic_system_word
  val variable = upper_word

  val source =
    general_term //dag_source> | <internal_source> | <external_source> | <unknown>

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

  val optional_info = opt("," ~ useful_info)
  val useful_info = general_term_list

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

  val include = kw("include") ~ "(" ~ file_name ~ formula_selection ~ ")" ~ "."
  val formula_selection = opt("," ~ "[" ~ name_list ~ "]")
  val name_list = rep1sep(name, delim(","))

  val general_term = general_list | general_data ~ opt(":" ~ general_term)
  val general_data =
    atomic_word ~ opt("(" ~ general_arguments ~ ")") | number | distinct_object
  val general_arguments = rep1sep(general_term, delim(","))
  val general_list = "[" ~ opt(general_term_list) ~ "]"
  val general_term_list = rep1sep(general_term, delim(","))

  val name = atomic_word | unsigned_integer
  val atomic_word = lower_word | single_quoted

  val atomic_system_word = lower_word | dollar_dollar_word
  val number = real | signed_integer | unsigned_integer

  val file_name = atomic_word

  val real = accept(RealKind)
  val unsigned_integer = accept(UnsignedKind)
  val signed_integer = accept(SignedKind)

  val dollar_dollar_word = accept(DollarWordKind)
  val lower_word = accept(LowerWordKind)
  val upper_word = accept(UpperWordKind)

  val single_quoted = accept(SingleQuotedKind)

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
