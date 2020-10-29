package mathgraph.repl
import CommandLexer._
import io.AnsiColor._
import mathgraph.corelogic._
import mathgraph.printer._

object Commands {

  case class LogicState(
      logicGraph: LogicGraph,
      printer: Printer,
      previousState: Option[LogicState]
  )

  abstract class Command {
    def apply(currentState: LogicState): LogicState = {
      System.out.println(s"${RED}Command not implemented${RESET}")
      currentState
    }
  }

  case object Help extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println("List of commands:")
      CommandLexer.keywords.foreach(kw => System.out.println(s"$kw"))
      currentState
    }
  }

  case object Leave extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println("Closing application")
      currentState
    }
  }

  case object Lse extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println("Lse command recognized")
      currentState
    }
  }

  case object Lss extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println("Lss command recognized")
      currentState
    }
  }

  case object Ls extends Command {

    def buildList(
        state: LogicState
    ): IndexedSeq[(Int, Option[Boolean], String)] = {
      for (pos <- 0 until state.logicGraph.size)
        yield (
          pos,
          state.logicGraph.getTruthOf(pos),
          state.printer.toAdvString(pos)
        )
    }

    def lineToString(pos: Int, truth: Option[Boolean], expr: String): String = {
      val truthStr = truth match {
        case None    => "\t\t"
        case Some(v) => s"[$v]\t"
      }
      pos.toString + " " + truthStr + " " + expr
    }

    override def apply(currentState: LogicState): LogicState = {

      System.out.println(
        buildList(currentState)
          .map { case (p, t, e) =>
            lineToString(p, t, e)
          }
          .mkString("\n")
      )

      currentState
    }
  }

  case object Absurd extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println("Absurd command recognized")
      currentState
    }
  }

  case class FixN(arg: Int) extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println(s"Fixn command recognized (arg = ${arg})")
      currentState
    }
  }

  case class Fix(arg1: Int, arg2: Int) extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println(s"Fix command recognized args = (${arg1}, ${arg2})")
      currentState
    }
  }

  case class Apply(arg: Int) extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println(s"Apply command recognized (arg = ${arg})")
      currentState
    }
  }

  case class Why(arg1: Int, arg2: Int) extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println(s"Why command recognized args = (${arg1}, ${arg2})")
      currentState
    }
  }

  case object FixAllTrue extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println("Fix all true, command recognized")
      currentState
    }
  }

  case object FixAllFalse extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println("Fix all false, command recognized")
      currentState
    }
  }

  case object Dij extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println("Dij command recognized")
      currentState
    }
  }

  case object Stats extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println("Stats command recognized")
      currentState
    }
  }

  case class Ctx(arg: Int) extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println(s"Ctx command recognized (arg = ${arg})")
      currentState
    }
  }

  case class Chain(arg: Int) extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println(s"Chain command recognized (arg = ${arg})")
      currentState
    }
  }

  case object Proof extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println("Proof command recognized")
      currentState
    }
  }

  case object Undo extends Command {
    override def apply(currentState: LogicState): LogicState = {
      currentState.previousState match {
        case Some(previousState) => {
          System.out.println("Undo successful")
          previousState
        }
        case None => {
          System.out.println("There is no operation to undo")
          currentState
        }
      }
    }
  }

  case object Clear extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.print("\u001b[H\u001b[2J");
      currentState
    }
  }

  case object UnknownCommand extends Command {
    override def apply(currentState: LogicState): LogicState = {
      System.out.println(s"Unknown command")
      currentState
    }
  }

  case class BadCommand(command: String, n: Int) extends Command {

    override def apply(currentState: LogicState): LogicState = {

      def printUsage(): Unit = n match {
        case 0 => System.out.println(s"usage: ${command}")
        case 1 => System.out.println(s"usage: ${command} pos")
        case _ if command == "fix" =>
          System.out.println(s"usage: ${command} next pos")
        case _ if command == "why" =>
          System.out.println(s"usage: ${command} a b")
        case _ => {
          UnknownCommand.apply(currentState)
          ()
        }
      }

      printUsage()
      currentState
    }
  }
}
