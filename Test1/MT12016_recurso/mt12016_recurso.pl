:-use_module('info.pl').

% EX1
raro(Movie) :-
  film(Movie, _, Dur, _),
  Dur < 60.
raro(Movie) :-
  film(Movie, _, Dur, _),
  Dur > 120.

% EX2
happierGuy(User1, User2, HappierGuy) :-
  vote(User1, Rates1),
  vote(User2, Rates2),
  sumRates(Rates1, SumRates1),
  sumRates(Rates2, SumRates2),
  length(Rates1, NumRates1),
  length(Rates2, NumRates2),
  Med1 is SumRates1 / NumRates1,
  Med2 is SumRates2 / NumRates2,
  happierGuyAux(User1, Med1, User2, Med2, HappierGuy).

happierGuyAux(User1, Med1, _, Med2, User1) :- Med1 > Med2.
happierGuyAux(_, Med1, User2, Med2, User2) :- Med1 < Med2.

sumRates([_-Rate|Rates], Res) :-
  sumRates(Rates, Res1),
  Res is Res1 + Rate.
sumRates([], 0).

% EX3
maxList([], Max, Max).
maxList([_-H|L], CurrMax, Max) :-
  H > CurrMax,
  maxList(L, H, Max).
maxList([_-H|L], CurrMax, Max) :-
  H =< CurrMax,
  maxList(L, CurrMax, Max).

likedBetter(User1, User2) :-
  vote(User1, Rates1),
  maxList(Rates1, 0, Max),
  vote(User2, Rates2),
  likedBetterAux(Rates2, Max).

likedBetterAux([], _).
likedBetterAux([_-H|T], Max) :-
  H < Max,
  likedBetterAux(T, Max).

% EX4
recommends(User, Movie) :-
  vote(User, Seen),
  vote(User2, Seen2), User \= User2,
  recommendsAux(Seen, Seen2),
  member(Movie-_, Seen2),
  \+ member(Movie-_, Seen), !.

recommendsAux([], _).
recommendsAux([M1-_|Seen1], Seen2) :-
  member(M1-_, Seen2),
  recommendsAux(Seen1, Seen2).

% EX5
invert(PredicateSymbol, Arity) :-
  length(L, Arity),
  F =.. [PredicateSymbol|L],
  F,
  retract(F),
  invert(PredicateSymbol, Arity),
  assert(F).
invert(_, _).

% EX6
onlyOne(User1, User2, OnlyOneList) :-
  vote(User1, Rate1),
  vote(User2, Rate2),
  findall(M, ((member(M-_, Rate1), \+member(M-_, Rate2)); (member(M-_, Rate2), \+member(M-_, Rate2))), OnlyOneList).

% EX7
:-dynamic filmUsersVotes/2.

filmVoting :-
  retractall(filmUsersVotes(_, _)),
  filmVotingAux.

filmVotingAux :-
  film(Movie, _, _, _),
  findall(User-Rate, (vote(User, UserRates), member(Movie-Rate, UserRates)), Rates),
  assert(filmUsersVotes(Movie, Rates)),
  fail.
filmVotingAux.

% EX8
dumpDataBase(FileName) :-
  tell(FileName),
  user(A, B, C),
  write(user(A, B, C)), nl,
  fail.
dumpDataBase(_) :-
  film(A, B, C, D),
  write(film(A, B, C, D)), nl,
  fail.
dumpDataBase(_) :-
  vote(A, B),
  write(vote(A, B)), nl,
  fail.
dumpDataBase(_) :- told.
