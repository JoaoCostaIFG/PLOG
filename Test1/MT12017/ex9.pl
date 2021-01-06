:-include('info.pl').

mostEffectivePlayers(Game, Players) :-
  maxEfficiency(Game, MaxEff, 0, []),
  findall(Player, (played(Player, Game, Hours, Perc), Eff is Perc/Hours, MaxEff = Eff), Players).

maxEfficiency(Game, MaxEff, CurrMax, Visited) :-
  played(Player, Game, Hours, Perc),
  \+member(Player, Visited),
  CurrMax1 is Perc / Hours,
  maxEfficiencyAux(CurrMax, CurrMax1, NewMax), !,
  maxEfficiency(Game, MaxEff, NewMax, [Player|Visited]).
maxEfficiency(_, CurrMax, CurrMax, _).
  
maxEfficiencyAux(CurrMax, CurrMax1, CurrMax) :- CurrMax >= CurrMax1.
maxEfficiencyAux(CurrMax, CurrMax1, CurrMax1) :- CurrMax =< CurrMax1.
