:-include('info.pl').

listGamesOfCategory(Cat) :-
  game(Game, Cats, Age),
  member(Cat, Cats),
  format('~w (~w)', [Game, Age]), nl,
  fail.
listGamesOfCategory(_).
