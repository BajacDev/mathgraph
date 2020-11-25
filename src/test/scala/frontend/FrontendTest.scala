import org.scalatest.funsuite.AnyFunSuite
import mathgraph.frontend.Trees._
import mathgraph.util._
import mathgraph.frontend._
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

  def frontend = Lexer andThen Parser andThen PrettyPrinter
  def nullOutputStream = new OutputStream {
    def write(b: Int): Unit = {}
  }

  def shouldSucceed(input: String, result: String): Unit = {
    assertResult(result) {
      Console.withErr(nullOutputStream) {
        frontend.run(StringSource("input", input))(new Context)
      }
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
    "let x := 1;" ~> Success("let x := 1;")
    "let x(arg) := 1;" ~> Success("let x(arg) := 1;")
    "let ||(a, b) := not(a) -> b;" ~> Success(
      "let ||(a, b) := ((a -> false) -> b);"
    )
    "let a, b;" ~> Success("let a;\nlet b;")
  }

  test("expressions are parsed correctly") {
    "a -> false -> c;" ~> Success("(a -> (false -> c));")
    "in(N, x) -> true;" ~> Success("(in(N, x) -> true);")
    "forall x. has(x, y) -> in(y, x);" ~> Success(
      "(forall x. (has(x, y) -> in(y, x)));"
    )
    "forall x y z. P(x, y) -> Q(y, z);" ~> Success(
      "(forall x y z. (P(x, y) -> Q(y, z)));"
    )
  }

  test("syntactic sugar is desugared") {
    "not a -> b;" ~> Success("((a -> false) -> b);")
    "exists x. P(x);" ~> Success("((forall x. (P(x) -> false)) -> false);")
    "forall x. forall y. P(x, y);" ~> Success("(forall x y. P(x, y));")
    "exists x. exists y. P(x, y);" ~> Success(
      "((forall x y. (P(x, y) -> false)) -> false);"
    )
    "not (not P(x));" ~> Success("P(x);")
    "not true;" ~> Success("false;")
  }

  test("invalid syntax is not parsed") {
    "let a := x" ~> Failure // missing semicolon
    "(a -> b" ~> Failure // unclosed parenthesis
    "= -> (;" ~> Failure // illegal characters
    "a + b;" ~> Failure // infix operators not allowed
  }

  test("comments are ignored") {
    "let a := x; // first line \n plus(a, b);" ~> Success(
      "let a := x;\nplus(a, b);"
    )
    "let a /* hello */ := x;" ~> Success("let a := x;")
    "let a /* := x;" ~> Failure // unclosed comment
  }
}
