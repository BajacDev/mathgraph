package mathgraph.repl

import CommandLexer._
import io.AnsiColor._
import mathgraph.corelogic._
import mathgraph.printer._
import mathgraph.solver._

object Commands {

  /** Represents the type of parameters to commands (can be extended to floats, booleans, ...) */
  abstract class ParamType
  case object IntT extends ParamType {
    override def toString: String = "int"
  }
  case object StringT extends ParamType {
    override def toString: String = "string"
  }

  /** A command takes a logic state and some parameters and returns the new state */
  type Command = LogicState => Seq[Any] => LogicState

  /** Generic class used to define commands */
  case class CommandDef private (
      name: String,
      desc: String,
      mandatoryParams: Seq[ParamType],
      optionalParams: Seq[ParamType],
      command: Command
  ) {
    def ~>(params: ParamType*): CommandDef =
      copy(mandatoryParams = params)
    def ~>?(params: ParamType*): CommandDef =
      copy(optionalParams = params)
    def ??(str: String): CommandDef = copy(desc = str)
  }

  object CommandDef {
    def apply(name: String, command: Command): CommandDef =
      CommandDef(name, "", Seq(), Seq(), command)
  }

  // This is used to align all the commands based on the longest command name
  lazy val maxCommandLength = commands.keys.map(_.length).max

  // Transforms a sequence of definitions into a map from name to command def
  private def cmdsToMap(defs: CommandDef*): Map[String, CommandDef] =
    defs.map(df => df.name -> df).toMap

  // Prints a short summary of what the given command does
  def printSummary(df: CommandDef): Unit = {
    val padded = df.name.padTo(maxCommandLength, ' ')
    val alignedDesc =
      df.desc.replaceAll("\n", "\n" + " " * (maxCommandLength + 4))
    println(s"- $padded  $alignedDesc")
  }

  // Prints a detailed description of the given command
  def printUsage(df: CommandDef): Unit = {
    val mandatory = df.mandatoryParams.map(tpe => s"<$tpe>")
    val optional = df.optionalParams.map(tpe => s"[<$tpe>]")
    println((df.name +: (mandatory ++ optional)).mkString(" "))
    println(df.desc)
  }

  // Prints the logic state to the console
  def printState(ls: LogicState, simple: Boolean): Unit = {
    val lg = ls.logicGraph
    val printer: Int => String =
      if (simple) ls.printer.toSimpleString(lg, _)
      else ls.printer.toString(lg, _)

    for (e <- 0 until lg.size) {
      val truth = lg
        .getTruthOf(e)
        .map(t => if (t) "[true]  " else "[false]")
        .getOrElse("       ")
      println(f"$e%04d $truth ${printer(e)}")
    }
  }

  // Helper function to create commands
  def transformState(f: LogicState => LogicState): Command = { ls =>
    { case Seq() =>
      f(ls)
    }
  }
  def consumeState(f: LogicState => Unit): Command = transformState { ls =>
    f(ls); ls
  }

  // -------------------------------------------------------------
  // Command defintions. Add new commands here.
  // -------------------------------------------------------------

  val help: Command = { ls =>
    {
      case Seq() =>
        println("Available commands :")
        commands.values.foreach(printSummary)
        ls
      case Seq(cmd: String) =>
        commands
          .get(cmd)
          .map(printUsage)
          .getOrElse(println(s"Command '$cmd' does not exist"))
        ls
    }
  }

  val lss: Command = consumeState { ls =>
    printState(ls, simple = true)
  }

  val ls: Command = consumeState { ls =>
    printState(ls, simple = false)
  }

  val absurd: Command = consumeState { ls =>
    val status = if (ls.logicGraph.isAbsurd) "absurd" else "not absurd"
    println(s"The logic graph is $status")
  }

  val fixN: Command = { ls =>
    { case Seq(e: Int) =>
      ls.copy(logicGraph = ls.logicGraph.fixLetSymbol(e)._1)
    }
  }

  val fix: Command = { ls =>
    { case Seq(e1: Int, e2: Int) =>
      ls.copy(logicGraph = ls.logicGraph.fix(e1, e2)._1)
    }
  }

  val simplify: Command = { ls =>
    { case Seq(e: Int) =>
      ls.copy(logicGraph = ls.logicGraph.simplifyInferenceRule(e)._1)
    }
  }

  val fixAllTrue: Command = transformState { ls =>
    ls.copy(logicGraph = Solver.fixAll(ls.logicGraph))
  }

  val fixAllFalse: Command = transformState { ls =>
    ls.copy(logicGraph = Solver.fixLetSym(ls.logicGraph))
  }

  val proof: Command = consumeState { ls =>
    ls.printer.proofAbsurd(ls.logicGraph).foreach(println)
  }

  val saturate: Command = transformState { ls =>
    proof(ls.copy(logicGraph = Solver.saturation(ls.logicGraph)))(Seq())
  }

  val stats: Command = consumeState { ls =>
    val lg = ls.logicGraph
    val printer = ls.printer
    val exprs = lg.getAllTruth
    val stats = Solver.getStats(lg, exprs)

    stats.foreach { case (ctx, set) =>
      println(
        s"Context(head: ${ctx.head} ${printer.toString(lg, ctx.head)}, idArg: ${ctx.idArg})"
      )
      set.map(printer.toString(lg, _)).foreach(println)
      println()
    }
  }

  val undo: Command = transformState { ls =>
    ls.previousState match {
      case Some(prev) =>
        println("Undo successful")
        prev
      case None =>
        println("Nothing to undo")
        ls
    }
  }

  /** Contains the list of all the available commands in the REPL */
  val commands: Map[String, CommandDef] = cmdsToMap(
    CommandDef("help", help) ~>? StringT ??
      """Provides help about commands. Type 'help' for general help
        |or 'help <cmd>' for help about a specific command.""".stripMargin,
    CommandDef("exit", ls => _ => ls) ??
      "Exits the REPL",
    CommandDef("lss", lss) ??
      "Displays all the expression in a simple way.",
    CommandDef("ls", ls) ??
      "Displays all the expression.",
    CommandDef("abs", absurd) ??
      "Displays whether the set of expressions is absurd.",
    CommandDef("fixn", fixN) ~> IntT ??
      "Fixes the given symbol with its let-symbol.",
    CommandDef("fix", fix) ~> (IntT, IntT) ??
      "Fixes the two symbols given as arguments.",
    CommandDef("simp", simplify) ~> IntT ??
      "Simplifies the given expression.",
    CommandDef("fat", fixAllTrue) ??
      "Fixes all the expressions to true.",
    CommandDef("faf", fixAllFalse) ??
      "Fixes all the expressions to false.",
    CommandDef("proof", proof) ??
      "Displays a proof by contradiction, if one was found.",
    CommandDef("sat", saturate) ??
      "Applies the saturation algorithm to the set of expressions.",
    CommandDef("stats", stats) ??
      "Displays statistics about the expressions."
  )
}
