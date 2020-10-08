e_primo(2).
e_primo(3).
e_primo(N) :-
  N > 3,
  N mod 2 =\= 0,
  \+has_factor(N, 3).

has_factor(N, D) :- N mod D =:= 0.
has_factor(N, D) :-
  D * D < N,
  D2 is D + 2,
  has_factor(N, D2).
