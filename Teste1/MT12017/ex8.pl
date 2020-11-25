:-include('info.pl').

averageAge(Game, AverageAge) :-
  findall(Player, played(Player, Game, _, _), Players),
  averageAgeAux(Players, AgeSum),
  length(Players, PlayerNum),
  AverageAge is AgeSum / PlayerNum.

averageAgeAux([], 0).
averageAgeAux([P|Players], AgeSum) :-
  averageAgeAux(Players, AgeSum1),
  player(_, P, Age),
  AgeSum is AgeSum1 + Age.
