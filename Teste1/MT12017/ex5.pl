:-dynamic played/4.
:-include('info.pl').

updatePlayer(Player, Game, Hours, Percentage) :-
  retract(played(Player, Game, CurrHours, CurrPerc)),
  NewHours is CurrHours + Hours,
  NewPerc is CurrPerc + Percentage,
  assert(played(Player, Game, NewHours, NewPerc)).
updatePlayer(Player, Game, Hours, Percentage) :-
  assert(played(Player, Game, Hours, Percentage)).
