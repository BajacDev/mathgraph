package mathgraph.repl

import mathgraph.util._
import Commands._
import CommandTokens._
import io.AnsiColor._
import scala.io.StdIn.readLine

object Repl {
  case class InvalidCommand(error: String, pos: Position) extends Exception
  object EmptyCommand extends Exception

  def checkArguments(df: CommandDef, args: Seq[Token]): Unit = {
    def error(msg: String, pos: Position): Nothing =
      throw InvalidCommand(
        s"$msg. To get help about '${df.name}', you can type 'help ${df.name}'.",
        pos
      )

    def rec(types: Seq[ParamType], args: Seq[Token], idx: Int): Unit =
      (types, args) match {
        case (_, Seq()) =>
        case (tpe +: _, (tk: StringToken) +: _) if tpe != StringT =>
          error(
            s"Argument number $idx of '${df.name}' should be of type $tpe, but a string was given",
            tk.pos
          )
        case (tpe +: _, (tk: IntToken) +: _) if tpe != IntT =>
          error(
            s"Argument number $idx of '${df.name}' should be of type $tpe, but an int was given",
            tk.pos
          )
        case (tpe +: _, (tk: ErrorToken) +: _) =>
          error(s"Invalid token for argument number $idx", tk.pos)
        case (_ +: typesTail, _ +: argsTail) =>
          rec(typesTail, argsTail, idx + 1)
      }

    val numMandatory = df.mandatoryParams.length
    val maxArguments = df.optionalParams.length + numMandatory

    if (args.length < numMandatory)
      error(
        s"'${df.name}' takes at least $numMandatory arguments, but ${args.length} were given",
        NoPosition
      )

    if (args.length > maxArguments)
      error(
        s"'${df.name}' takes at most $maxArguments arguments, but ${args.length} were given",
        NoPosition
      )

    rec(df.mandatoryParams ++ df.optionalParams, args, 1)
  }

  def parseCommand(tokens: Seq[Token]): (CommandDef, Seq[Any]) = {
    if (tokens.isEmpty)
      throw EmptyCommand

    tokens match {
      case Seq() =>
        throw EmptyCommand
      case StringToken(cmd) +: args if commands.contains(cmd) =>
        val df = commands(cmd)
        checkArguments(df, args)

        val castArgs = args.map {
          case IntToken(value)    => value
          case StringToken(value) => value
        }

        (df, castArgs)
      case (tk @ StringToken(cmd)) +: args =>
        throw InvalidCommand(s"Command '$cmd' does not exist.", tk.pos)
      case tk +: rest =>
        throw InvalidCommand(
          "A command must start with a command name.",
          tk.pos
        )
    }
  }

  def run(initialState: LogicState): Unit = {
    System.out.print(s"${GREEN}>>> ${RESET}")
    val tokens = CommandLexer(readLine())

    try {
      val (df, args) = parseCommand(tokens)

      if (df.name == "exit")
        return

      run(df.command(initialState)(args))
    } catch {
      case InvalidCommand(error, pos) =>
        if (pos != NoPosition)
          println(s"    ${" " * (pos.col - 1)}^")
        println(error)
        run(initialState)
      case EmptyCommand =>
        run(initialState)
    }
  }
}
