package mathgraph.repl

import Commands._
import mathgraph.util._
import io.AnsiColor._
import scala.io.StdIn

object Repl {

  def apply(currentState: LogicState)(implicit ctx: Context): LogicState = {

    val pipeline = CommandLexer andThen CommandParser

    System.out.print(s"${GREEN}>>> ${RESET}")

    val command = pipeline.run(StdIn.readLine())(ctx)

    command match {
      case Leave => currentState
      case _ => Repl(command.apply(currentState))
    }
  }
}
