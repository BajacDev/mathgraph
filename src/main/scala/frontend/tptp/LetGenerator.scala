package mathgraph.frontend.tptp

/* TODO detect free identifiers and introduce Let for them
 *
 * fof(..., axiom, (! [Y] : p(Y) -> q(Y) )).
 * p and q are never introduced but mathgraph would need:
 * let p;
 * let q;
 * forall Y. p(Y) -> q(Y);
 *
 */
