package mathgraph.frontend.tptp
import mathgraph.util._
import mathgraph.frontend.TPTPTrees._
import mathgraph.frontend._
import scala.language.implicitConversions
import Tokens._
import scallion._

object Parser extends Parsers with Pipeline[Iterator[Token], Program] {

  import Implicits._

  type Kind = TokenKind
  type Token = Tokens.Token

  override def getKind(token: Token): TokenKind = kindOf(token)

  def kw(str: String): Syntax[Token] = elem(KwKind(str))
  def op(str: String): Syntax[Token] = elem(OperatorKind(str))
  def pred(str: String): Syntax[Token] = elem(PredicateKind(str))
  def delim(str: String): Syntax[Token] = elem(DelimKind(str))

  val eof: Syntax[Token] = elem(EOFKind)

  implicit def skipped(str: String): Skip = elem(DelimKind(str)).skip

  lazy val tptpFile: Syntax[Program] = (many(tptpInput) ~ eof.skip).map {
    lines =>
      val includes: Seq[Include] = lines.collect { case inc: Include =>
        inc
      }

      val formulas = lines.collect { case a: Annotated =>
        a
      }

      Program(includes, formulas)
  }

  lazy val tptpInput: Syntax[Tree] = annotatedFormula | include

  lazy val annotatedFormula = fofAnnotated | cnfAnnotated

  lazy val fofAnnotated: Syntax[Tree] = (kw(
    "fof"
  ) ~ "(" ~ name ~ "," ~ formulaRole ~ "," ~ fofFormula ~ annotations ~ ")" ~ ".")
    .map {
      case fof ~ ((name, _)) ~ (("conjecture", _)) ~ fofFormula ~ annotations =>
        Conjecture(name, fofFormula).setPos(fof)
      case fof ~ ((name, _)) ~ role ~ fofFormula ~ annotations =>
        Axiom(name, fofFormula).setPos(fof)
    }

  lazy val cnfAnnotated: Syntax[Tree] = (kw(
    "cnf"
  ) ~ "(" ~ name ~ "," ~ formulaRole ~ "," ~ cnfFormula ~ annotations ~ ")" ~ ".")
    .map {
      case cnf ~ ((name, _)) ~ (("conjecture", _)) ~ cnfFormula ~ annotations =>
        Conjecture(name, cnfFormula).setPos(cnf)
      case cnf ~ ((name, _)) ~ role ~ cnfFormula ~ annotations =>
        Axiom(name, cnfFormula).setPos(cnf)
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
        And(Implies(lhs, rhs).setPos(lhs), Implies(rhs, lhs).setPos(rhs))
          .setPos(lhs)

      case lhs ~ Some((OperatorToken("<~>"), Seq(rhs))) =>
        And(
          Or(lhs, rhs).setPos(lhs),
          Or(Not(lhs).setPos(lhs), Not(rhs).setPos(rhs)).setPos(lhs)
        ).setPos(lhs)

      case lhs ~ Some((OperatorToken("&"), rhs +: more)) =>
        (
          more
            .foldLeft(And(lhs, rhs).setPos(lhs))((acc, next) =>
              And(acc, next).setPos(acc)
            )
          )
          .setPos(lhs)
      case lhs ~ Some((OperatorToken("|"), rhs +: more)) =>
        (
          more
            .foldLeft(Or(lhs, rhs).setPos(lhs))((acc, next) =>
              Or(acc, next).setPos(acc)
            )
          )
          .setPos(lhs)

      case lhs ~ Some((OperatorToken("~&"), Seq(rhs))) =>
        Not(And(lhs, rhs).setPos(lhs)).setPos(lhs)
      case lhs ~ Some((OperatorToken("~|"), Seq(rhs))) =>
        Not(Or(lhs, rhs).setPos(lhs)).setPos(lhs)
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
      case lhs ~ PredicateToken("=") ~ rhs => Equals(lhs, rhs).setPos(lhs)
      case lhs ~ PredicateToken("!=") ~ rhs =>
        Not(Equals(lhs, rhs).setPos(lhs)).setPos(lhs)
      case _ ~ _ ~ _ => ???
    }

  lazy val plainSystemOrDefinedInfix =
    (plainOrSystemTerm ~ opt(definedInfixPred ~ term)).map {
      case t ~ None                              => t
      case lhs ~ Some(PredicateToken("=") ~ rhs) => Equals(lhs, rhs).setPos(lhs)
      case lhs ~ Some(PredicateToken("!=") ~ rhs) =>
        Not(Equals(lhs, rhs).setPos(lhs)).setPos(lhs)
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

  lazy val distinctObject: Syntax[(String, Position)] =
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
      case (filename, pos) ~ None => Include(filename, Seq()).setPos(pos)
      case (filename, pos) ~ Some(selection) =>
        Include(filename, selection.map(_._1)).setPos(pos)
    }
  lazy val formulaSelection = opt("," ~ "[" ~ nameList ~ "]")
  lazy val nameList = rep1sep(name, delim(","))

  lazy val generalTerm: Syntax[(String, Position)] =
    recursive(generalList | generalDataTerm)

  lazy val generalData: Syntax[(String, Position)] =
    atomicWordWithArguments | number | distinctObject

  lazy val generalDataTerm: Syntax[(String, Position)] =
    (generalData ~ opt(":" ~ generalTerm)).map {
      case (data, pos) ~ None            => (data, pos)
      case (data, pos) ~ Some((term, _)) => (data + ":" + term, pos)
    }

  lazy val atomicWordWithArguments: Syntax[(String, Position)] =
    (atomicWord ~ opt("(" ~ generalArguments ~ ")")).map {
      case (word, pos) ~ None => (word, pos)
      case (word, pos) ~ Some(args) =>
        (word + "(" + args.mkString(",") + ")", pos)
    }

  lazy val generalList: Syntax[(String, Position)] =
    (delim("[") ~ opt(generalTermList) ~ delim("]")).map {
      case delim ~ None ~ _ => ("[]", delim.pos)
      case delim ~ Some(terms) ~ _ =>
        ("[" + terms.mkString(",") + "]", delim.pos)
    }

  lazy val generalArguments: Syntax[Seq[String]] =
    rep1sep(generalTerm, delim(",")).map { case args =>
      args.map(_._1)
    }
  lazy val generalTermList: Syntax[Seq[String]] =
    rep1sep(generalTerm, delim(",")).map { case args =>
      args.map(_._1)
    }

  lazy val name = atomicWord | unsignedInteger
  lazy val atomicWord: Syntax[(String, Position)] =
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

  lazy val dollarDollarWord: Syntax[(String, Position)] =
    accept(DollarWordKind) { case tk @ DollarWordToken(word) =>
      (word, tk.pos)
    }

  lazy val lowerWord: Syntax[(String, Position)] = accept(LowerWordKind) {
    case tk @ WordToken(word) => (word, tk.pos)
  }

  lazy val upperWord: Syntax[(String, Position)] = accept(UpperWordKind) {
    case tk @ WordToken(word) => (word, tk.pos)
  }

  lazy val singleQuoted: Syntax[(String, Position)] =
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

  protected def apply(tokens: Iterator[Token])(ctxt: Context): Program = {
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
