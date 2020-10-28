not(X) :- X, !, fail.
not(X).

unificavel([], _, []).

unificavel([H1|T1], Termo, T2) :-
  not(H1=Termo), !,
  unificavel(T1, Termo, T2).

unificavel([H|T1], Termo, [H|T2]) :-
  unificavel(T1, Termo, T2).

