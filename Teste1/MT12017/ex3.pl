:-include('info.pl').

mysumlist([], 0).
mysumlist([H|T], Sum) :-
  mysumlist(T, Sum1),
  Sum is Sum1 + H.

hasPlayed(Player, Game, Hours) :-
  played(Player, Game, Hours, _).
hasPlayed(Player, Game, 0) :-
  \+played(Player, Game, _, _).

mymaplist(_, [], []).
mymaplist(Player, [H|T], [Hours|Out]) :-
  hasPlayed(Player, H, Hours),
  mymaplist(Player, T, Out).

timePlayingGames(Player, Games, ListOfTimes, SumTimes) :-
  mymaplist(Player, Games, ListOfTimes),
  mysumlist(ListOfTimes, SumTimes).
