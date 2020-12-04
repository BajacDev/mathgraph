package mathgraph.frontend.mgl

import org.scalatest.funsuite.AnyFunSuite
import mathgraph.frontend.BackendTrees._
import mathgraph.util._
import mathgraph.frontend.mgl._
import java.io.OutputStream

class FrontendTests extends AnyFunSuite {
  object PrettyPrinter extends Pipeline[Program, String] {
    def apply(prog: Program)(ctxt: Context): String = {
      def rec(tree: Tree): String = tree match {
        case Program(lets, axioms) =>
          (lets ++ axioms).map(rec).mkString("", ";\n", ";")
        case Let(name, Seq(), bodyOpt) =>
          s"let $name" + bodyOpt.map(bd => " := " + rec(bd)).getOrElse("")
        case Let(name, vars, bodyOpt) =>
          s"let $name(${vars.mkString(", ")})" + bodyOpt
            .map(bd => " := " + rec(bd))
            .getOrElse("")
        case Implies(lhs, rhs) => s"(${rec(lhs)} -> ${rec(rhs)})"
        case True              => "true"
        case False             => "false"
        case Apply(id, Seq())  => id
        case Apply(id, args)   => s"$id(${args.map(rec).mkString(", ")})"
        case Forall(ids, body) => s"(forall ${ids.mkString(" ")}. ${rec(body)})"
      }

      rec(prog)
    }
  }

  def frontend = Lexer andThen Parser andThen NameAnalyzer andThen PrettyPrinter
  def nullOutputStream = new OutputStream {
    def write(b: Int): Unit = {}
  }

  def shouldSucceed(input: String, result: String): Unit = {
    assertResult(result) {
      frontend.run(StringSource("input", input))(new Context)
    }
  }

  def shouldFail(input: String): Unit = {
    assertThrows[FatalError] {
      Console.withErr(nullOutputStream) {
        frontend.run(StringSource("input", input))(new Context)
      }
    }
  }

  abstract class FrontendResult
  case class Success(result: String) extends FrontendResult
  case object Failure extends FrontendResult

  implicit class WithInput(input: String) {
    def ~>(result: FrontendResult): Unit = result match {
      case Success(result) => shouldSucceed(input, result)
      case Failure         => shouldFail(input)
    }
  }

  test("definitions are parsed correctly") {
    "let x;" ~> Success("let x;")
    "let x(arg);" ~> Success("let x(arg);")
    "let 1, x := 1;" ~> Success("let 1;\nlet x := 1;")
    "let 1, x(arg) := 1;" ~> Success("let 1;\nlet x(arg) := 1;")
    "let a, b;" ~> Success("let a;\nlet b;")
  }

  test("expressions are parsed correctly") {
    "let a, c; a -> a -> c;" ~> Success(
      "let a;\nlet c;\n(a -> (a -> c));"
    )
    "let in(a, b), N, x; in(N, x) -> x;" ~> Success(
      "let in(a, b);\nlet N;\nlet x;\n(in(N, x) -> x);"
    )
    "let has(a, b), in(a, b); forall x y. has(x, y) -> in(y, x);" ~> Success(
      "let has(a, b);\nlet in(a, b);\n(forall x y. (has(x, y) -> in(y, x)));"
    )
    "let P(a, b), Q(a, b); forall x y z. P(x, y) -> Q(y, z);" ~> Success(
      "let P(a, b);\nlet Q(a, b);\n(forall x y z. (P(x, y) -> Q(y, z)));"
    )
  }

  test("syntactic sugar is desugared") {
    "let a, b; ~a -> b;" ~> Success("let a;\nlet b;\n((a -> false) -> b);")
    "let a; ~~a;" ~> Success("let a;\na;")
    "let P(x); exists x. P(x);" ~> Success(
      "let P(x);\n((forall x. (P(x) -> false)) -> false);"
    )
    "let P(x, y); forall x. forall y. P(x, y);" ~> Success(
      "let P(x, y);\n(forall x y. P(x, y));"
    )
    "let P(x, y); exists x. exists y. P(x, y);" ~> Success(
      "let P(x, y);\n((forall x y. (P(x, y) -> false)) -> false);"
    )
    "~true;" ~> Success("false;")
    "let x; x -> true;" ~> Success("let x;\ntrue;")
    "let x; false -> x;" ~> Success("let x;\ntrue;")
    "let x; true -> x;" ~> Success("let x;\nx;")
    "let x; (x -> false) -> false;" ~> Success("let x;\nx;")
    "forall x. forall y. x;" ~> Success("(forall x y. x);")
    "forall x. forall y. true;" ~> Success("true;")
  }

  test("invalid syntax is not parsed") {
    "let x, a := x" ~> Failure // missing semicolon
    "(a -> b" ~> Failure // unclosed parenthesis
    "= -> (;" ~> Failure // illegal characters
    "a + b;" ~> Failure // infix operators not allowed
  }

  test("comments are ignored") {
    "let a, b, plus(x, y); // first line \n plus(a, b);" ~> Success(
      "let a;\nlet b;\nlet plus(x, y);\nplus(a, b);"
    )
    "let a /* hello */ := true;" ~> Success("let a := true;")
    "let a /* := true;" ~> Failure // unclosed comment
  }

  test("operators are correctly parsed") {
    "let(left, 20) a + b, (left, 30) a * b, (right, 40) a ^ b, 1, 2, 3; 1 + 2 * 3 + 2 ^ 2 ^ 2 * 2 + 1;" ~> Success(
      "let +(a, b);\nlet *(a, b);\nlet ^(a, b);\nlet 1;\nlet 2;\nlet 3;\n+(+(+(1, *(2, 3)), *(^(2, ^(2, 2)), 2)), 1);"
    )
    "let(left, 5) a & b, (left, 15) a | b, a, b, c; a -> b & c; a -> b | c;" ~> Success(
      "let &(a, b);\nlet |(a, b);\nlet a;\nlet b;\nlet c;\n&((a -> b), c);\n(a -> |(b, c));"
    )
  }

  test("invalid names are rejected") {
    "let a(x, x);" ~> Failure
    "let a, a;" ~> Failure
    "let a + b, x + y;" ~> Failure
    "a -> b;" ~> Failure
    "forall x x. true;" ~> Failure
  }

  test("invalid operators are rejected") {
    "let(abc, 1) a + b;" ~> Failure
    "let(left, abc) a + b;" ~> Failure
    "true + false;" ~> Failure
    "let(left, 10) a -> b;" ~> Failure
    "let(left, 10) a ~ b;" ~> Failure
  }
}
