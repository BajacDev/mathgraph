package mathgraph.repl

import CommandLexer._
import io.AnsiColor._
import mathgraph.corelogic._
import mathgraph.printer._
import mathgraph.solver._

object Commands {

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

  def lineToString(pos: Int, truth: Option[Boolean], expr: String): String = {
    val truthStr = truth match {
      case None    => "\t\t"
      case Some(v) => s"[$v]\t"
    }
    pos.toString + " " + truthStr + " " + expr
  }

  def buildLines(
      logicGraph: LogicGraph,
      exprs: Iterable[Int],
      printFunc: Int => String
  ): Iterable[(Int, Option[Boolean], String)] = {
    exprs.map(pos =>
      (
        pos,
        logicGraph.getTruthOf(pos),
        printFunc(pos)
      )
    )
  }

  case object Lss extends Command {

    override def apply(currentState: LogicState): LogicState = {
      val lg = currentState.logicGraph
      val printer = currentState.printer
      buildLines(lg, 0 until lg.size, printer.toSimpleString(lg, _))
        .map { case (p, t, e) =>
          lineToString(p, t, e)
        }
        .foreach(System.out.println)

      currentState
    }
  }

  case object Ls extends Command {

    override def apply(currentState: LogicState): LogicState = {
      val lg = currentState.logicGraph
      val printer = currentState.printer
      buildLines(lg, 0 until lg.size, printer.toString(lg, _))
        .map { case (p, t, e) =>
          lineToString(p, t, e)
        }
        .foreach(System.out.println)

      currentState
    }
  }

  case object Absurd extends Command {
    override def apply(currentState: LogicState): LogicState = {
      val logicGraph = currentState.logicGraph
      System.out.println(s"Absurd: ${logicGraph.isAbsurd}")
      currentState
    }
  }

  case class FixN(arg: Int) extends Command {
    override def apply(currentState: LogicState): LogicState = {
      val logicGraph = currentState.logicGraph
      val (lg, pos) = logicGraph.fixLetSymbol(arg)
      currentState.copy(logicGraph = lg)
    }
  }

  case class Fix(arg1: Int, arg2: Int) extends Command {
    override def apply(currentState: LogicState): LogicState = {
      val logicGraph = currentState.logicGraph
      val (lg, pos) = logicGraph.fix(arg1, arg2)
      currentState.copy(logicGraph = lg)
    }
  }

  case class Simplify(arg: Int) extends Command {
    override def apply(currentState: LogicState): LogicState = {
      val logicGraph = currentState.logicGraph
      val (lg, pos) = logicGraph.symplifyInferenceRule(arg)
      currentState.copy(logicGraph = lg)
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
      val logicGraph = currentState.logicGraph
      val lg = Solver.fixAll(logicGraph)
      currentState.copy(logicGraph = lg)
    }
  }

  case object FixAllFalse extends Command {
    override def apply(currentState: LogicState): LogicState = {
      val logicGraph = currentState.logicGraph
      val lg = Solver.fixLetSym(logicGraph)
      currentState.copy(logicGraph = lg)
    }
  }

  case object Saturate extends Command {
    override def apply(currentState: LogicState): LogicState = {
      val lg = Solver.saturation(currentState.logicGraph)
      val newCtx = currentState.copy(logicGraph = lg)
      Proof.apply(newCtx)
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
      val lg = currentState.logicGraph
      val printer = currentState.printer
      printer.proofAbsurd(lg).foreach(System.out.println)
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
