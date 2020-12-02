package mathgraph.frontend
import scala.collection.mutable.HashMap

/** Represents the signature of a symbol */
abstract class SymbolSig(arity: Int)

/** Signature of normal symbols */
case class NormalSig(val arity: Int) extends SymbolSig(arity)

/** Some symbols are inlined, and should not be printed */
case class InlinedSig(val arity: Int) extends SymbolSig(arity)

/** Signature of operator symbols */
case class OperatorSig(val rightAssociative: Boolean, val precedence: Int)
    extends SymbolSig(2)

/** The symbol table gather the signature of all the symbols defined in the program */
class SymbolTable {
  // Stores the name to identifier mapping of expressions
  private val nameToId = HashMap[String, Identifier]()

  // Stores the signature of all the symbols in a program
  private val symbols = HashMap[Identifier, SymbolSig]()

  /** Adds a symbol to the program */
  def addSymbol(
      name: String,
      arity: Int,
      inlined: Boolean = false
  ): Identifier = {
    val id = Identifier.fresh(name)
    val sig = if (inlined) InlinedSig(arity) else NormalSig(arity)
    nameToId += name -> id
    symbols += id -> sig
    id
  }

  /** Sets a given symbol as an operator symbol with the given associativity and precedence.
    * This is not necessary, but used to inform the printer that a certain symbol can be printed
    * as an infix binary operator.
    */
  def useAsOperator(
      id: Identifier,
      rightAssociative: Boolean,
      precedence: Int
  ): Unit = {
    val previousSig = symbols.getOrElse(
      id,
      throw new IllegalArgumentException(s"Symbol $id does not exist")
    )
    previousSig match {
      case NormalSig(2) =>
        symbols(id) = OperatorSig(rightAssociative, precedence)
      case _ =>
        throw new IllegalArgumentException(
          "Only non-inlined symbols with an arity of 2 can be used as operators"
        )
    }
  }
}
