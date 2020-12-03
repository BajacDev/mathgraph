package mathgraph.util
import io.AnsiColor._
import scala.collection.mutable.Map
import scala.io.Source

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

  def info(msg: String, pos: Positioned): Unit = info(msg, pos.pos)
  def warning(msg: String, pos: Positioned): Unit = warning(msg, pos.pos)
  def error(msg: String, pos: Positioned): Unit = error(msg, pos.pos)
  def fatal(msg: String, pos: Positioned): Nothing = fatal(msg, pos.pos)

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

      val lines = getLines(pos.file)
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
  private def getLines(file: String): IndexedSeq[String] = {
    filesToLines.get(file) match {
      case Some(lines) =>
        lines

      case None =>
        try {
          val src = Source.fromFile(file)
          val lines = src.getLines().toIndexedSeq
          src.close()
          filesToLines += file -> lines // cache the result for the next lookups
          lines
        } catch {
          case _: java.io.FileNotFoundException => IndexedSeq()
        }
    }
  }
}
