import org.scalatest.funsuite.AnyFunSuite
import mathgraph.repl._
import CommandTokens._

class ReplTest extends AnyFunSuite {
  implicit class InputDecorator(val input: String) {
    def yieldsTokens(result: Token*): Unit = {
      assertResult(result) {
        CommandLexer(input)
      }
    }

    def isValidCommand(name: String, args: Any*): Unit = {
      val (df, actualArgs) = Repl.parseCommand(CommandLexer(input))
      assertResult(name)(df.name)
      assertResult(args)(actualArgs) 
    }

    def isInvalidCommand: Unit = {
      assertThrows[Repl.InvalidCommand] {
        Repl.parseCommand(CommandLexer(input))
      }
    }
  }

  test("command lexer is correct") {
    "   " yieldsTokens ()
    " 123   456  " yieldsTokens (IntToken(123), IntToken(456))
    " str   12  lk" yieldsTokens (StringToken("str"), IntToken(12), StringToken("lk"))
    " +''*  abc12" yieldsTokens (StringToken("+''*"), StringToken("abc12"))
    "fix 1 2" yieldsTokens (StringToken("fix"), IntToken(1), IntToken(2))
  }

  test("valid commands are parsed") {
    "help" isValidCommand ("help")
    "help cmd" isValidCommand ("help", "cmd")
    "fix 1 2" isValidCommand ("fix", 1, 2)
    "exit" isValidCommand ("exit")
    "simp 37" isValidCommand ("simp", 37)
    "fat" isValidCommand ("fat")
  }

  test("invalid commands are rejected") {
    "1 cmd".isInvalidCommand // command must start with string
    "unknown 1 2".isInvalidCommand // unknown command
    "help cmd 2".isInvalidCommand // two many args
    "help 1".isInvalidCommand // wrong type
    "fix 1".isInvalidCommand // not enough args
  }
}
