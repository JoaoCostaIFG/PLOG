% fills L1 with N numbers in [1, M] and L2 with N-1 numbers in [1, M]
% each number in L2 is the sum of two numbers in L1

:-use_module(library(lists)).
:-use_module(library(between)).

prog1(N, M, L1, L2) :-
  length(L1, N),
  N1 is N-1, length(L2, N1),
  findall(E, between(1, M, E), LE),
  fill(L1, LE, LE_),
  fill(L2, LE_, _),
  check(L1, L2).

fill([], LEf, LEf).
fill([X|Xs], LE, LEf) :-
  select(X, LE, LE_),
  fill(Xs, LE_, LEf).

check([_], []).
check([A,B|R], [X|Xs]) :-
  A+B =:= X,
  check([B|R], Xs).
