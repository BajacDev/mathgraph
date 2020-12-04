package mathgraph

package object frontend {
  val MGLFrontend = mgl.Lexer andThen mgl.Parser andThen mgl.NameAnalyzer
  val TPTPFrontend = tptp.ResolvingParser andThen tptp.NameAnalyzer
}
