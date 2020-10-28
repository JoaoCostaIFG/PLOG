% Quando X == Y. e X e Y são maiores que Z. Neste caso, o programa respondiria
% que Z era o maior dos 3.

max(X, Y, Z, X) :- X >= Y, X >= Z, !.
max(X, Y, Z, Y) :- Y >= X, Y >= Z, !.
max(_, _, Z, Z).
