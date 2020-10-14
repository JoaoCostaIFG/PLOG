% a (caso base = [] => no)

% membro(X, [X1 | T1]) :-
%   L \= [],
%   (X1 == X; membro(X, T1)).

% membro(X, [X | _]).
% membro(X, [Y | L]) :-
  % X \= Y,
  % membro(X, L).

% b

membro(X, L) :- append(_, [X | _], L).

% c

last(L, X) :- append(_, [X], L).

% d

nmember([H | _], 0, X) :- X is H.
nmember([_ | T], N, X) :-
  N >= 0,
  N1 is N - 1,
  nmember(T, N1, X1),
  X is X1.

