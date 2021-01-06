:-use_module(library(clpfd)).
:-use_module(library(lists)).

prog2(N, M, L1, L2) :-
  length(L1, N),
  N1 is N-1, length(L2, N1),
  domain(L1, 1, M),
  domain(L2, 1, M),
  append(L1, L2, L3), all_distinct(L3),
  check(L1, L2),
  labeling([], L1).

check([_], []).
check([A,B|R], [X|Xs]) :-
  A #< B,
  A + B #= X,
  check([B|R], Xs).
