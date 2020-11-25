:-include('info.pl').

achievedALot(Player) :-
  played(Player, _, _, Perc),
  Perc > 80 .
