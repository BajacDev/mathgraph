include('tptp_test02.p', [axiom_2]).

fof(axiom_1, axiom, (! [X] : $true)).
fof(conj, conjecture, (? [A] : $true)).
