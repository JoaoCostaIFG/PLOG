% a)

test(A, B).

myfunctor(Term, F, Arity) :-
  Term=..[F|Args],
  length(Args, Arity).

% b)

myarg(N, Term, Arg) :-
  Term=..[_|Args],
  write(Term),
  npos(Args, N, Arg).

npos([H|_], 0, Elem) :- H = Elem.
npos([_|T], N, Elem) :-
  N > 0,
  N1 is N - 1,
  npos(T, N1, Elem).
