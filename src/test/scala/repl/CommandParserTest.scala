import org.scalatest.funsuite.AnyFunSuite
import mathgraph.util._
import mathgraph.repl._
import mathgraph.repl.Commands._
import scala.language.implicitConversions

import scala.util.Random

class CommandParserTest extends AnyFunSuite {

  val pipeline = CommandLexer andThen CommandParser

  val ctx = new Context()

  case class TestOutput(command: Command) {

    def expect(predicate: Command => Boolean): Unit = {
      if (!predicate(command))
        fail(s"unexpected command ${command}")
    }

    def ==>(expected: Command): Unit = expect(_ == expected)

    def ?(): Unit = expect {
      case BadCommand(_, _) => true
      case UnknownCommand   => true
      case _                => false
    }

    def !(expected: (String, Int)): Unit = expect(
      _ == BadCommand(expected._1, expected._2)
    )
  }

  implicit def parse(input: String): TestOutput = {
    TestOutput(pipeline.run(input)(ctx))
  }

  test("unit commands are parsed correctly") {
    "help" ==> Help
    "leave" ==> Leave
    "lse" ==> Lse
    "lss" ==> Lss
    "ls" ==> Ls
    "absurd" ==> Absurd
    "fat" ==> FixAllTrue
    "faf" ==> FixAllFalse
    "s" ==> Saturate
    "stats" ==> Stats
    "proof" ==> Proof
    "undo" ==> Undo
    "clear" ==> Clear
  }

  test("consumer commands are parsed correctly") {
    "fixn 1" ==> FixN(1)
    "simplify 1" ==> Simplify(1)
    "ctx 1" ==> Ctx(1)
    "chain 1" ==> Chain(1)

    "simplify 12" ==> Simplify(12)
    "ctx 27" ==> Ctx(27)
    "chain 999" ==> Chain(999)

    "fixn 01" ==> FixN(1)
    "fixn 0" ==> FixN(0)
    "fixn 000" ==> FixN(0)
    "fixn 5" ==> FixN(5)
  }

  test("biconsumer commands are parsed correctly") {
    "fix 0 0" ==> Fix(0, 0)
    "fix 1 2" ==> Fix(1, 2)
    "fix 402 0" ==> Fix(402, 0)
    "fix 56 12" ==> Fix(56, 12)
    "fix 10 91" ==> Fix(10, 91)

    "why 675 2" ==> Why(675, 2)
    "why 1 567" ==> Why(1, 567)
    "why 12467 23467" ==> Why(12467, 23467)
    "why 1287 0" ==> Why(1287, 0)
  }

  test("wrong usage of unit commands is detected correctly") {
    "help 0" ! ("help", 0)
    "leave 1" ! ("leave", 0)
    "lse 45" ! ("lse", 0)
    "fat 0 1" ! ("fat", 0)

    "lss lss" ! ("lss", 0)
    "ls help" ! ("ls", 0)
    "proof fix" ! ("proof", 0)
    "proof fix 1 2" ! ("proof", 0)

    "ls fixn 1" ! ("ls", 0)
    "absurd 123 faf" ! ("absurd", 0)
    "faf 1 proof 1" ! ("faf", 0)
  }

  test("wrong usage of consumer commands is detected correctly") {
    "fixn 1 2" ! ("fixn", 1)
    "simplify 123 21371" ! ("simplify", 1)
    "ctx 1 12 13" ! ("ctx", 1)
    "chain 13 33" ! ("chain", 1)

    "fixn" ! ("fixn", 1)
    "simplify" ! ("simplify", 1)
    "ctx" ! ("ctx", 1)
    "chain" ! ("chain", 1)

    "fixn chain" ! ("fixn", 1)
    "fixn absurd" ! ("fixn", 1)
    "fixn why" ! ("fixn", 1)
    "fixn 5 why" ! ("fixn", 1)
    "fixn fixn 123" ! ("fixn", 1)
    "fixn proof 0" ! ("fixn", 1)
    "fixn 1 help" ! ("fixn", 1)
  }

  test("wrong usage of biconsumer commands is detected correctly") {
    "fix" ! ("fix", 2)
    "why" ! ("why", 2)

    "fix 1" ! ("fix", 2)
    "why 1" ! ("why", 2)

    "fix 12376" ! ("fix", 2)
    "why 12376" ! ("why", 2)

    "fix 1231 12873 123" ! ("fix", 2)
    "why 1423 23 436" ! ("why", 2)

    "fix fix" ! ("fix", 2)
    "fix why" ! ("fix", 2)
    "fix fix 12" ! ("fix", 2)
    "fix fix 12 123" ! ("fix", 2)
    "fix why 12" ! ("fix", 2)
    "fix absurd" ! ("fix", 2)
    "fix simplify" ! ("fix", 2)
    "fix simplify 1" ! ("fix", 2)

    "why why" ! ("why", 2)
    "why fix" ! ("why", 2)
    "why fix 12" ! ("why", 2)
    "why fix 12 123" ! ("why", 2)
    "why why 12" ! ("why", 2)
    "why proof" ! ("why", 2)
    "why simplify" ! ("why", 2)
    "why simplify 1" ! ("why", 2)
  }

  def randomAlphanumeric(): Char = {
    Random.alphanumeric(0)
  }

  def randomLetter(): Char = randomAlphanumeric() match {
    case c if (c.isDigit) => randomLetter()
    case c                => c
  }

  def randomNonKeyword(nextChar: () => Char, maxLength: Int): String = {

    def randomAcc(acc: String, length: Int): String = {
      if (length <= 0)
        acc
      else
        randomAcc(acc + nextChar(), length - 1)
    }

    val random = randomAcc("", Random.nextInt(maxLength))

    if (CommandLexer.keywords.contains(random))
      randomNonKeyword(nextChar, maxLength)
    else
      random
  }

  test("random strings lead to unkown commands") {
    val nbTests = 100
    val maxLength = 20

    for (i <- 0 until nbTests) yield {
      randomNonKeyword(randomLetter, maxLength) ? ()
      randomNonKeyword(randomAlphanumeric, maxLength) ? ()
    }
  }
}
