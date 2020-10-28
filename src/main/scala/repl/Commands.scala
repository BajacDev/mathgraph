package mathgraph.repl
import io.AnsiColor._
import mathgraph.corelogic._
import mathgraph.printer._
import CommandLexer._

object Commands {

  abstract class Command {
    def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println(s"${RED}Command not implemented${RESET}")
      (current, previous)
    }
  }

  case object Help extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println("List of commands:")
      CommandLexer.keywords.foreach(kw => System.out.println(s"$kw"))
      (current, previous)
    }
  }

  case object Leave extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println("Closing application")
      (current, previous)
    }
  }

  case object Lse extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println("Lse command recognized")
      (current, previous)
    }
  }

  case object Lss extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println("Lss command recognized")
      (current, previous)
    }
  }

  case object Ls extends Command {

    def buildList(current: LogicGraph): IndexedSeq[(Int, Option[Boolean], String)] = {
      val printer = Printer.init(current)
      for (pos <- 0 until current.size)
        yield (pos, current.getTruthOf(pos), printer.toAdvString(pos))
    }

    def lineToString(pos: Int, truth: Option[Boolean], expr: String): String = {
      val truthStr = truth match {
        case None    => "\t\t"
        case Some(v) => s"[$v]\t"
      }
      pos.toString + " " + truthStr + " " + expr
    }

    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {

      System.out.println(
        buildList(current)
          .map { case (p, t, e) =>
            lineToString(p, t, e)
          }
          .mkString("\n")
      )

      (current, previous)
    }
  }

  case object Absurd extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println("Absurd command recognized")
      (current, previous)
    }
  }

  case class FixN(arg: Int) extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println(s"Fixn command recognized (arg = ${arg})")
      (current, previous)
    }
  }

  case class Fix(arg1: Int, arg2: Int) extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println(s"Fix command recognized args = (${arg1}, ${arg2})")
      (current, previous)
    }
  }

  case class Apply(arg: Int) extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println(s"Apply command recognized (arg = ${arg})")
      (current, previous)
    }
  }

  case class Why(arg1: Int, arg2: Int) extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println(s"Why command recognized args = (${arg1}, ${arg2})")
      (current, previous)
    }
  }

  case object FixAllTrue extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println("Fix all true, command recognized")
      (current, previous)
    }
  }

  case object FixAllFalse extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println("Fix all false, command recognized")
      (current, previous)
    }
  }

  case object Dij extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println("Dij command recognized")
      (current, previous)
    }
  }

  case object Stats extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println("Stats command recognized")
      (current, previous)
    }
  }

  case class Ctx(arg: Int) extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println(s"Ctx command recognized (arg = ${arg})")
      (current, previous)
    }
  }

  case class Chain(arg: Int) extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println(s"Chain command recognized (arg = ${arg})")
      (current, previous)
    }
  }

  case object Proof extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println("Proof command recognized")
      (current, previous)
    }
  }

  case object Undo extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      previous match {
        case p :: ps => {
          System.out.println("Undo successful")
          (p, ps)
        }
        case Nil => {
          System.out.println("There is no operation to undo")
          (current, previous)
        }
      }
    }
  }

  case object UnknownCommand extends Command {
    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {
      System.out.println(s"Unknown command")
      (current, previous)
    }
  }

  case class BadCommand(command: String, n: Int) extends Command {

    override def apply(current: LogicGraph, previous: List[LogicGraph]): (LogicGraph, List[LogicGraph]) = {

      def printUsage(): Unit = n match {
        case 0 => System.out.println(s"usage: ${command}")
        case 1 => System.out.println(s"usage: ${command} pos")
        case _ if command == "fix" => System.out.println(s"usage: ${command} next pos")
        case _ if command == "why" => System.out.println(s"usage: ${command} a b")
        case _ => {
          UnknownCommand.apply(current, previous)
          ()
        }
      }

      printUsage()
      (current, previous)
    }
  }
}
