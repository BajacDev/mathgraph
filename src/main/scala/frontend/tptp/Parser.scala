package mathgraph.frontend.tptp
import mathgraph.util._
import mathgraph.frontend.Trees._
import scala.language.implicitConversions
import Tokens._
import scallion._

object Parser extends Parsers with Pipeline[Iterator[Token], Seq[Tree]] {

  import Implicits._

  type Kind = TokenKind
  type Token = Tokens.Token

  case class TPTPInclude(filename: String, formulas: Seq[(String, Position)])
      extends Tree {
    def unapply(tree: Tree): Option[(String, Seq[(String, Position)])] =
      tree match {
        case TPTPInclude(filename, formulas) => Some((filename, formulas))
        case _                               => None
      }
  }

  override def getKind(token: Token): TokenKind = kindOf(token)

  def kw(str: String): Syntax[Token] = elem(KwKind(str))
  def op(str: String): Syntax[Token] = elem(OperatorKind(str))
  def pred(str: String): Syntax[Token] = elem(PredicateKind(str))
  def delim(str: String): Syntax[Token] = elem(DelimKind(str))

  val eof: Syntax[Token] = elem(EOFKind)

  implicit def skipped(str: String): Skip = elem(DelimKind(str)).skip

  lazy val tptpFile: Syntax[Seq[Tree]] = (many(tptpInput) ~ eof.skip).map {
    case inputs =>
      inputs
  }

  lazy val tptpInput: Syntax[Tree] = annotatedFormula | include

  lazy val annotatedFormula = (fofAnnotated | cnfAnnotated).map {
    case formula => formula
  }

  lazy val fofAnnotated: Syntax[Tree] = (kw(
    "fof"
  ) ~ "(" ~ name ~ "," ~ formulaRole ~ "," ~ fofFormula ~ annotations ~ ")" ~ ".")
    .map {
      case fof ~ name ~ formulaRole ~ fofFormula ~ annotations => {

        val body = formulaRole match {
          case ("conjecture", pos) => Implies(fofFormula, False).setPos(fof)
          case _                   => fofFormula.setPos(fof)
        }

        Let(name._1, Seq(), Some(body)).setPos(fof)
      }
    }

  lazy val cnfAnnotated: Syntax[Tree] = (kw(
    "cnf"
  ) ~ "(" ~ name ~ "," ~ formulaRole ~ "," ~ cnfFormula ~ annotations ~ ")" ~ ".")
    .map { case cnf ~ name ~ formulaRole ~ cnfFormula ~ annotations =>
      val body = formulaRole match {
        case ("conjecture", pos) => Implies(cnfFormula, False).setPos(cnf)
        case _                   => cnfFormula.setPos(cnf)
      }

      Let(name._1, Seq(), Some(body)).setPos(cnf)
    }

  lazy val annotations = opt("," ~ source ~ optionalInfo).map { case _ =>
    ()
  }

  lazy val formulaRole = lowerWord

  lazy val fofFormula: Syntax[Expr] =
    (unitaryFormula ~ opt(assocBinaryOp | nonAssocBinaryOp)).map {
      case unitary ~ None => unitary
      case lhs ~ Some((OperatorToken("=>"), Seq(rhs))) =>
        Implies(lhs, rhs).setPos(lhs)
      case lhs ~ Some((OperatorToken("<="), Seq(rhs))) =>
        Implies(rhs, lhs).setPos(lhs)
      case lhs ~ Some((OperatorToken("<=>"), Seq(rhs))) =>
        And(Implies(lhs, rhs), Implies(rhs, lhs)).setPos(lhs)

      case lhs ~ Some((OperatorToken("<~>"), Seq(rhs))) =>
        And(Or(lhs, rhs), Or(Not(lhs), Not(rhs))).setPos(lhs)

      case lhs ~ Some((OperatorToken("&"), rhs +: more)) =>
        (
          more.foldLeft(And(lhs, rhs))((acc, next) => And(acc, next))
        ).setPos(lhs)
      case lhs ~ Some((OperatorToken("|"), rhs +: more)) =>
        (more.foldLeft(Or(lhs, rhs))((acc, next) => Or(acc, next))).setPos(lhs)

      case lhs ~ Some((OperatorToken("~&"), Seq(rhs))) =>
        Not(And(lhs, rhs)).setPos(lhs)
      case lhs ~ Some((OperatorToken("~|"), Seq(rhs))) =>
        Not(Or(lhs, rhs)).setPos(lhs)
      case _ ~ _ => ???
    }

  lazy val assocBinaryOp: Syntax[(Token, Seq[Expr])] =
    orFormula | andFormula

  lazy val orFormula: Syntax[(Token, Seq[Expr])] = many1(moreOrFormula).map {
    case formulas => (OperatorToken("|"), formulas)
  }
  lazy val moreOrFormula = op("|").skip ~ unitaryFormula

  lazy val andFormula: Syntax[(Token, Seq[Expr])] =
    many1(moreAndFormula).map { case formulas =>
      (OperatorToken("&"), formulas)
    }
  lazy val moreAndFormula = op("&").skip ~ unitaryFormula

  lazy val binaryConnective =
    op("<=>") | op("=>") | op("<=") | op("<~>") | op("~|") | op("~&")

  lazy val nonAssocBinaryOp: Syntax[(Token, Seq[Expr])] =
    (binaryConnective ~ unitaryFormula).map { case connective ~ formula =>
      (connective, Seq(formula))
    }

  lazy val unitaryFormula: Syntax[Expr] =
    recursive(
      quantifiedFormula | unaryFormula | "(" ~ fofFormula ~ ")" | atomicFormula
    )

  lazy val quantifiedFormula: Syntax[Expr] =
    (quantifier ~ "[" ~ variableList ~ "]" ~ ":" ~ unitaryFormula)
      .map {
        case (op @ OperatorToken("!")) ~ variables ~ formula =>
          Forall(variables, formula).setPos(op)
        case (op @ OperatorToken("?")) ~ variables ~ formula =>
          Exists(variables, formula).setPos(op)
        case _ ~ _ ~ _ => ???
      }

  lazy val quantifier = op("!") | op("?")

  lazy val variableList = rep1sep(variable, delim(",")).map { case variables =>
    variables.map(_._1)
  }

  lazy val unaryFormula: Syntax[Expr] = (op("~") ~ unitaryFormula).map {
    case op ~ formula => Not(formula).setPos(op)
  }

  lazy val cnfFormula = "(" ~ disjunction ~ ")" | disjunction
  lazy val disjunction: Syntax[Expr] =
    rep1sep(literal, op("|")).map { case lits =>
      (
        lits.tail.foldLeft(lits.head)((acc, next) => Or(acc, next))
      ).setPos(lits.head)
    }

  lazy val literal: Syntax[Expr] =
    (atomicFormula | negatedAtomicFormula).map { case formula =>
      formula
    }
  lazy val negatedAtomicFormula = (op("~") ~ atomicFormula).map {
    case op ~ formula => Not(formula).setPos(op)
  }

  lazy val atomicFormula = yes | no | plainSystemDefinedAtom

  lazy val arguments = rep1sep(term, delim(",")).map { case args =>
    args
  }

  lazy val yes: Syntax[Expr] = pred("$true").map { case tk =>
    True.setPos(tk)
  }
  lazy val no: Syntax[Expr] = pred("$false").map { case tk =>
    False.setPos(tk)
  }

  lazy val plainSystemDefinedAtom: Syntax[Expr] =
    (definedTermVariableInfix | plainSystemOrDefinedInfix)

  lazy val definedInfixPred = pred("=") | pred("!=")
  lazy val definedTermVariableInfix =
    ((definedTerm | termVariable) ~ definedInfixPred ~ term).map {
      case lhs ~ PredicateToken("=") ~ rhs  => Equals(lhs, rhs).setPos(lhs)
      case lhs ~ PredicateToken("!=") ~ rhs => Not(Equals(lhs, rhs)).setPos(lhs)
      case _ ~ _ ~ _                        => ???
    }

  lazy val plainSystemOrDefinedInfix =
    (plainOrSystemTerm ~ opt(definedInfixPred ~ term)).map {
      case t ~ None                              => t
      case lhs ~ Some(PredicateToken("=") ~ rhs) => Equals(lhs, rhs).setPos(lhs)
      case lhs ~ Some(PredicateToken("!=") ~ rhs) =>
        Not(Equals(lhs, rhs)).setPos(lhs)
      case _ ~ _ => ???
    }

  lazy val term = recursive(functionTerm | termVariable)

  lazy val termVariable: Syntax[Expr] = variable.map { case (id, pos) =>
    Apply(id, Seq()).setPos(pos)
  }

  lazy val functionTerm = definedTerm | plainOrSystemTerm

  lazy val plainOrSystemTerm: Syntax[Expr] = recursive(
    (lowerWord | singleQuoted | dollarDollarWord) ~ opt(
      "(" ~ arguments ~ ")"
    )
  ).map {
    case (word, pos) ~ None       => Apply(word, Seq()).setPos(pos)
    case (word, pos) ~ Some(args) => Apply(word, args).setPos(pos)
  }

  lazy val definedTerm: Syntax[Expr] = (number | distinctObject).map {
    case (id, pos) =>
      Apply(id, Seq()).setPos(pos)
  }

  lazy val distinctObject: Syntax[(Identifier, Position)] =
    accept(DistinctObjectKind) { case tk @ DistinctObjectToken(distinct) =>
      (distinct, tk.pos)
    }

  lazy val systemTerm: Syntax[Expr] =
    (atomicSystemWord ~ opt("(" ~ arguments ~ ")")).map {
      case (word, pos) ~ None       => Apply(word, Seq()).setPos(pos)
      case (word, pos) ~ Some(args) => Apply(word, args).setPos(pos)
    }
  lazy val variable = upperWord

  lazy val source = generalTerm

  lazy val optionalInfo = opt("," ~ usefulInfo)
  lazy val usefulInfo = generalTermList

  lazy val include: Syntax[Tree] =
    (kw("include").skip ~ "(" ~ fileName ~ formulaSelection ~ ")" ~ ".").map {
      case (filename, pos) ~ None => TPTPInclude(filename, Seq()).setPos(pos)
      case (filename, pos) ~ Some(selection) =>
        TPTPInclude(filename, selection).setPos(pos)
    }
  lazy val formulaSelection = opt("," ~ "[" ~ nameList ~ "]")
  lazy val nameList = rep1sep(name, delim(","))

  lazy val generalTerm: Syntax[(Identifier, Position)] =
    recursive(generalList | generalDataTerm)

  lazy val generalData: Syntax[(Identifier, Position)] =
    atomicWordWithArguments | number | distinctObject

  lazy val generalDataTerm: Syntax[(Identifier, Position)] =
    (generalData ~ opt(":" ~ generalTerm)).map {
      case (data, pos) ~ None            => (data, pos)
      case (data, pos) ~ Some((term, _)) => (data + ":" + term, pos)
    }

  lazy val atomicWordWithArguments: Syntax[(Identifier, Position)] =
    (atomicWord ~ opt("(" ~ generalArguments ~ ")")).map {
      case (word, pos) ~ None => (word, pos)
      case (word, pos) ~ Some(args) =>
        (word + "(" + args.mkString(",") + ")", pos)
    }

  lazy val generalList: Syntax[(Identifier, Position)] =
    (delim("[") ~ opt(generalTermList) ~ delim("]")).map {
      case delim ~ None ~ _ => ("[]", delim.pos)
      case delim ~ Some(terms) ~ _ =>
        ("[" + terms.mkString(",") + "]", delim.pos)
    }

  lazy val generalArguments: Syntax[Seq[Identifier]] =
    rep1sep(generalTerm, delim(",")).map { case args =>
      args.map(_._1)
    }
  lazy val generalTermList: Syntax[Seq[Identifier]] =
    rep1sep(generalTerm, delim(",")).map { case args =>
      args.map(_._1)
    }

  lazy val name = atomicWord | unsignedInteger
  lazy val atomicWord: Syntax[(Identifier, Position)] =
    lowerWord | singleQuoted

  lazy val atomicSystemWord = lowerWord | dollarDollarWord
  lazy val number = real | signedInteger | unsignedInteger

  lazy val fileName = atomicWord

  lazy val real = accept(RealKind) { case tk @ RealToken(value) =>
    (value, tk.pos)
  }
  lazy val unsignedInteger = accept(UnsignedKind) {
    case tk @ UnsignedToken(value) => (value, tk.pos)
  }
  lazy val signedInteger = accept(SignedKind) { case tk @ SignedToken(value) =>
    (value, tk.pos)
  }

  lazy val dollarDollarWord: Syntax[(Identifier, Position)] =
    accept(DollarWordKind) { case tk @ DollarWordToken(word) =>
      (word, tk.pos)
    }

  lazy val lowerWord: Syntax[(Identifier, Position)] = accept(LowerWordKind) {
    case tk @ WordToken(word) => (word, tk.pos)
  }

  lazy val upperWord: Syntax[(Identifier, Position)] = accept(UpperWordKind) {
    case tk @ WordToken(word) => (word, tk.pos)
  }

  lazy val singleQuoted: Syntax[(Identifier, Position)] =
    accept(SingleQuotedKind) { case tk @ SingleQuotedToken(quoted) =>
      (quoted, tk.pos)
    }

  // Ensures the grammar is in LL(1), otherwise prints some counterexamples
  lazy val checkLL1: Boolean = {
    if (tptpFile.isLL1) {
      true
    } else {
      debug(tptpFile)
      false
    }
  }

  protected def apply(tokens: Iterator[Token])(ctxt: Context): Seq[Tree] = {

    val parser = Parser(tptpFile)

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
