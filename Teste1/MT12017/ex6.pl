:-include('info.pl').

appendIf(Player, [Game|Games], Visited) :-
  played(Player, Game, Hours, _),
  Hours < 10,
  \+ member(Game, Visited), !,
  appendIf(Player, Games, [Game|Visited]).
appendIf(_, [], _).

fewHours(Player, Games) :-
  appendIf(Player, Games, []).
