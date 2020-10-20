package mathgraph.util
import scala.util.parsing.input._
import io.AnsiColor._

// This exception will be thrown when a fatal error occurs when running the program
case class FatalError(msg: String) extends Exception(msg)

class Context {
  // Those are the reporting functionalities that the context provides
  def info(msg: String, pos: Position = NoPosition): Unit = 
    report(s"${GREEN}[ info  ]${RESET}", msg, pos)

  def warning(msg: String, pos: Position = NoPosition): Unit =
    report(s"${YELLOW}[warning]${RESET}", msg, pos)

  def error(msg: String, pos: Position = NoPosition): Unit = {
    hasErrors = true
    report(s"${RED}[ error ]${RESET}", msg, pos)
  }

  def fatal(msg: String, pos: Position = NoPosition): Nothing = {
    report(s"${RED}[ fatal ]${RESET}", msg, pos)
    throw FatalError(s"$pos: $msg")
  }

  def info(msg: String, pos: Positional): Unit = info(msg, pos.pos)
  def warning(msg: String, pos: Positional): Unit = warning(msg, pos.pos)
  def error(msg: String, pos: Positional): Unit = error(msg, pos.pos)
  def fatal(msg: String, pos: Positional): Nothing = fatal(msg, pos.pos)

  /** This terminates the program if any errors were reported */
  def terminateIfErrors() = {
    if (hasErrors) {
      fatal("There were errors.")
    }
  }

  // This indicates whether there were errors in the execution of the program
  private var hasErrors = false

  // Prints a message to stderr
  private def err(msg: String): Unit = {
    Console.err.println(msg)
  }

  // Reports a message with 
  private def report(prefix: String, msg: Any, pos: Position): Unit = {
    err(s"$prefix line ${pos.line}, column ${pos.column}: $msg")
    err(s"${pos.longString.split('\n').map(prefix + " " + _).mkString("\n")}")
  }
}
