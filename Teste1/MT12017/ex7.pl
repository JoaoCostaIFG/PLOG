:-include('info.pl').

%player(Name, UserName, Age).

ageRange(MinAge, MaxAge, Players) :-
  ageRangeAux(MinAge, MaxAge, Players, []).

ageRangeAux(MinAge, MaxAge, [P|Players], Visited) :-
  player(P, _, Age),
  Age >= MinAge, Age =< MaxAge,
  \+member(P, Visited), !,
  ageRangeAux(MinAge, MaxAge, Players, [P|Visited]).
ageRangeAux(_, _, [], _).

% Or
% ageRange(MinAge, MaxAge, Players) :-
  % findall(Name, (player(Name, _ Age), Age >= MinAge, Age =< MaxAge), Players).
