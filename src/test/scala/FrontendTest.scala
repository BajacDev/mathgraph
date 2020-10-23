import org.scalatest.funsuite.AnyFunSuite
import mathgraph.frontend.Trees._
import mathgraph.util._
import mathgraph.frontend._
import java.io.OutputStream

class ParserTest extends AnyFunSuite {
  object PrettyPrinter extends Pipeline[Program, String] {
    def apply(prog: Program)(ctxt: Context): String = {
      def rec(tree: Tree): String = tree match {
        case Program(lets, axioms) =>
          (lets ++ axioms).map(rec).mkString("", ";\n", ";")
        case Let(name, Seq(), bodyOpt) =>
          s"let $name" + bodyOpt.map(bd => " = " + rec(bd)).getOrElse("")
        case Let(name, vars, bodyOpt) =>
          s"let $name(${vars.mkString(", ")})" + bodyOpt
            .map(bd => " = " + rec(bd))
            .getOrElse("")
        case Implies(lhs, rhs) => s"(${rec(lhs)} -> ${rec(rhs)})"
        case True              => "true"
        case False             => "false"
        case Apply(id, Seq())  => id
        case Apply(id, args)   => s"$id(${args.map(rec).mkString(", ")})"
        case Forall(id, body)  => s"(forall $id. ${rec(body)})"
      }

      rec(prog)
    }
  }

  def frontend = Lexer andThen Parser andThen PrettyPrinter
  def nullOutputStream = new OutputStream {
    def write(b: Int): Unit = {}
  }

  def shouldSucceed(input: String, result: String): Unit = {
    try {
      Console.withErr(nullOutputStream) {
        assert(frontend.run(input)(new Context) == result)
      }
    } catch {
      case err: FatalError =>
        fail("Unexpected failure of the frontend: " + err, err)
    }
  }

  def shouldFail(input: String): Unit = {
    try {
      Console.withErr(nullOutputStream) {
        frontend.run(input)(new Context)
      }
      fail("The frontend was expected to fail.")
    } catch {
      case _: FatalError =>
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
    "let x = 1;" ~> Success("let x = 1;")
    "let x(arg) = 1;" ~> Success("let x(arg) = 1;")
  }

  test("expressions are parsed correctly") {
    "a -> false -> c;" ~> Success("(a -> (false -> c));")
    "in(N, x) -> true;" ~> Success("(in(N, x) -> true);")
    "forall x. has(x, y) -> in(y, x);" ~> Success(
      "(forall x. (has(x, y) -> in(y, x)));"
    )
  }

  test("syntactic sugar is desugared") {
    "not a -> b;" ~> Success("((a -> false) -> b);")
    "exists x. P(x);" ~> Success("((forall x. (P(x) -> false)) -> false);")
  }

  test("invalid syntax is not parsed") {
    "let a = x" ~> Failure // missing semicolon
    "(a -> b" ~> Failure // unclosed parenthesis
    "= -> (;" ~> Failure // illegal characters
    "a + b;" ~> Failure // infix operators not allowed
  }
}
