package mathgraph.frontend
import scala.collection.mutable.HashMap

/** Represents the signature of a symbol */
trait SymbolSig {
  def arity: Int
}

/** Signature of normal symbols. They can be marked as inlined if they should be inlined when printed. */
case class NormalSig(arity: Int, inlined: Boolean) extends SymbolSig

/** Signature of operator symbols */
case class OperatorSig(rightAssociative: Boolean, precedence: Int) extends SymbolSig {
  def arity = 2
}

/** The symbol table gather the signature of all the symbols defined in the program */
class SymbolTable {
  
  // Stores the name to identifier mapping
  private val nameToId = HashMap[String, Identifier]()

  // Stores the signature of all the symbols in a program
  private val symbols = HashMap[Identifier, SymbolSig]()

  /** Adds a mapping from symbol to identifier and signature in the table */
  private def addSymbol(name: String, id: Identifier, sig: SymbolSig): Unit = {
    nameToId += name -> id
    symbols += id -> sig
  }

  /** Adds a symbol to the table */
  def addSymbol(
      name: String,
      arity: Int,
      inlined: Boolean = false
  ): Identifier = {
    val id = Identifier.fresh(name)
    addSymbol(name, id, NormalSig(arity, inlined))
    id
  }

  /** Adds an infix binary operator to the table */
  def addOperator(name: String, rightAssociative: Boolean, precedence: Int): Identifier = {
    val id = Identifier.fresh(name)
    addSymbol(name, id, OperatorSig(rightAssociative, precedence))
    id
  }

  /** Retrieves a symbol signature and its identifier given a name */
  def getSymbol(name: String): Option[(Identifier, SymbolSig)] =
    for (id <- nameToId.get(name); sig <- symbols.get(id)) yield (id, sig)

  /** Retrieves a symbol given an identifier */
  def getSymbol(id: Identifier): SymbolSig = symbols(id)
}
