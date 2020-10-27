package mathgraph.repl
import io.AnsiColor._
import mathgraph.corelogic._
import mathgraph.printer._

object Commands {

  var logicGraph: LogicGraph = LogicGraph.init

  abstract class Command {
    def apply(): Unit = {
      System.out.println(s"${RED}Command not implemented${RESET}")
    }
  }

  case object Help extends Command {
    override def apply(): Unit = {
      System.out.println("List of commands:")
    }
  }

  case object Leave extends Command {
    override def apply(): Unit = {
      System.out.println("Closing application")
    }
  }

  case object Lse extends Command {
    override def apply(): Unit = {
      System.out.println("Lse command recognized")
    }
  }

  case object Lss extends Command {
    override def apply(): Unit = {
      System.out.println("Lss command recognized")
    }
  }

  case object Ls extends Command {

    def buildList = {
      val printer = Printer.init(logicGraph)
      for (pos <- 0 until logicGraph.size)
        yield (pos, logicGraph.getTruthOf(pos), printer.toAdvString(pos))
    }

    def lineToString(pos: Int, truth: Option[Boolean], expr: String) = {
      val truthStr = truth match {
        case None    => "\t\t"
        case Some(v) => s"[$v]\t"
      }
      pos.toString + " " + truthStr + " " + expr
    }

    override def apply(): Unit = {

      System.out.println(
        buildList
          .map { case (p, t, e) =>
            lineToString(p, t, e)
          }
          .mkString("\n")
      )
    }
  }

  case object Absurd extends Command {
    override def apply(): Unit = {
      System.out.println("Absurd command recognized")
    }
  }

  case class FixN(arg: Int) extends Command {
    override def apply(): Unit = {
      System.out.println(s"Fixn command recognized (arg = ${arg})")
    }
  }

  case class Fix(arg1: Int, arg2: Int) extends Command {
    override def apply(): Unit = {
      System.out.println(s"Fix command recognized args = (${arg1}, ${arg2})")
    }
  }

  case class Apply(arg: Int) extends Command {
    override def apply(): Unit = {
      System.out.println(s"Apply command recognized (arg = ${arg})")
    }
  }

  case class Why(arg1: Int, arg2: Int) extends Command {
    override def apply(): Unit = {
      System.out.println(s"Why command recognized args = (${arg1}, ${arg2})")
    }
  }

  case object FixAllTrue extends Command {
    override def apply(): Unit = {
      System.out.println("Fix all true, command recognized")
    }
  }

  case object FixAllFalse extends Command {
    override def apply(): Unit = {
      System.out.println("Fix all false, command recognized")
    }
  }

  case object Dij extends Command {
    override def apply(): Unit = {
      System.out.println("Dij command recognized")
    }
  }

  case object Stats extends Command {
    override def apply(): Unit = {
      System.out.println("Stats command recognized")
    }
  }

  case class Ctx(arg: Int) extends Command {
    override def apply(): Unit = {
      System.out.println(s"Ctx command recognized (arg = ${arg})")
    }
  }

  case class Chain(arg: Int) extends Command {
    override def apply(): Unit = {
      System.out.println(s"Chain command recognized (arg = ${arg})")
    }
  }

  case object Proof extends Command {
    override def apply(): Unit = {
      System.out.println("Proof command recognized")
    }
  }

  case object UnknownCommand extends Command {
    override def apply(): Unit = {
      System.out.println(s"Unknown command")
    }
  }

  case class BadCommand(command: String, n: Int) extends Command {
    override def apply(): Unit = {
      n match {
        case 0 => System.out.println(s"usage: ${command}")
        case 1 => System.out.println(s"usage: ${command} pos")
        case _ if command == "fix" =>
          System.out.println(s"usage: ${command} next pos")
        case _ if command == "why" =>
          System.out.println(s"usage: ${command} a b")
        case _ => UnknownCommand.apply()
      }
    }
  }
}
