package mathgraph.util
import io.AnsiColor._
import scala.collection.mutable.Map

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

  def info(msg: String, pos: Positioned): Unit = info(msg, pos.pos2)
  def warning(msg: String, pos: Positioned): Unit = warning(msg, pos.pos2)
  def error(msg: String, pos: Positioned): Unit = error(msg, pos.pos2)
  def fatal(msg: String, pos: Positioned): Nothing = fatal(msg, pos.pos2)

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
    if (pos != NoPosition) {
      err(s"$prefix $pos: $msg")

      val lines = getLines(pos.source)
      if (pos.line > 0 && pos.line - 1 < lines.size) {
        err(s"$prefix ${lines(pos.line - 1)}")
        err(s"$prefix ${" " * (pos.col - 1)}^")
      }
    } else {
      err(s"$prefix $msg")
    }
  }

  // This caches the file to lines mapping for error reporting
  private var filesToLines = Map[String, IndexedSeq[String]]()

  // Retrieves the lines of a given file from the cache or from the file itself
  private def getLines(source: AbstractSource): IndexedSeq[String] = {
    filesToLines.get(source.name) match {
      case Some(lines) =>
        lines

      case None =>
        val src = source.source
        val lines = src.getLines().toIndexedSeq
        src.close()
        filesToLines += source.name -> lines // cache the result for the next lookups
        lines
    }
  }
}
