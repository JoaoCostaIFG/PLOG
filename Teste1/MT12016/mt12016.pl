:-use_module('info.pl').

% EX1
curto(Movie) :-
  film(Movie, _, Dur, _),
  Dur < 125.

% EX2
diff(User1, User2, Difference, Film) :-
  vote(User1, Votes1),
  member(Film-Rate1, Votes1),
  vote(User2, Votes2),
  member(Film-Rate2, Votes2),
  Difference is abs(Rate1 - Rate2).

% EX3
niceGuy(User) :-
  vote(User, Votes),
  member(Film-Rate1, Votes),
  Rate1 >= 8,
  member(Film2-Rate2, Votes),
  Film \= Film2,
  Rate2 >= 8.

% EX4
elemsComuns(List1, Common, List2) :-
  length(List1, Len1),
  length(List2, Len2),
  listSmall(List1, List2, Len1, Len2, LSmall, LBig),
  elemsComunsAux(LSmall, LBig, Common, []).

elemsComunsAux([H|T], L, [H|Ret], Visited) :-
  \+member(H, Visited),
  member(H, L), !,
  elemsComunsAux(T, L, Ret, [H|Visited]).
elemsComunsAux([H|T], L, Ret, Visited) :-
  \+member(H, L),
  elemsComunsAux(T, L, Ret, [H|Visited]).
elemsComunsAux([], _, [], _).

listSmall(List1, List2, Len1, Len2, List2, List1) :- Len1 >= Len2.
listSmall(List1, List2, Len1, Len2, List1, List2) :- Len1 < Len2.

% EX5
printCategory(Category) :-
  film(Title, Categories, Duration, Rate),
  member(Category, Categories),
  format('~w (~wmin, ~w/10)', [Title, Duration, Rate]), nl,
  fail.
printCategory(_).

% EX6
similarity(Film1, Film2, Similarity) :-
  film(Film1, Categories1, Dur1, Rate1),
  film(Film2, Categories2, Dur2, Rate2),
  elemsComuns(Categories1, ComCate, Categories2),
  append(Categories1, Categories2, AllCatsDup),
  sort(AllCatsDup, AllCats),
  length(AllCats, NumCats),
  length(ComCate, NumComCats),
  Similarity is round((NumComCats/NumCats * 100 - 3 * abs(Dur1 - Dur2) - 5 * abs(Rate2 - Rate1)) * 100) / 100.

% EX7
:-use_module(library(lists)).

mostSimilar(Film, Similarity, Films) :-
  findall(Similarity, (similarity(Film, Film2, Similarity), Film \= Film2, Similarity > 10), Similarities),
  Similarities \= [],
  max_member(Similarity, Similarities), !,
  findall(Film2, (similarity(Film, Film2, Similarity), Film \= Film2), Films).
mostSimilar(_, 0, []).

% EX8
trimScore([M-_|Votes], [M|Movies]) :-
  trimScore(Votes, Movies).
trimScore([], []).

avgVoteDiff(User1, User2, Avg) :-
  vote(User1, Votes1),
  vote(User2, Votes2),
  trimScore(Votes1, Movies1),
  trimScore(Votes2, Movies2),
  elemsComuns(Movies1, CommonMovies, Movies2),
  avgVoteDiffAux(User1, User2, CommonMovies, Tot),
  length(CommonMovies, NumMovies),
  Avg is Tot / NumMovies.
avgVoteDiffAux(User1, User2, [M|Movies], Tot) :-
  diff(User1, User2, Difference, M),
  avgVoteDiffAux(User1, User2, Movies, Tot1),
  Tot is Difference + Tot1.
avgVoteDiffAux(_, _, [], 0).

distancia(User1, Distancia, User2) :-
  avgVoteDiff(User1, User2, VoteAvgDiff),
  user(User1, Age1, _),
  user(User2, Age2, _),
  countryDiff(User1, User2, CDiff),
  Distancia is VoteAvgDiff + abs(Age1 - Age2)/3 + CDiff.

countryDiff(User1, User2, 2) :-
  user(User1, _, C1),
  \+user(User2, _, C1).
countryDiff(User1, User2, 0) :-
  user(User1, _, C1),
  user(User2, _, C1).

% EX9
update(Film) :-
  retract(film(Film, Cat, Dur, _)),

  findall(Rate, (vote(_, ListOfFilm), member(Film-Rate, ListOfFilm)), Rates),
  length(Rates, RateNum),
  sumlist(Rates, RateTot),
  NewRate is RateTot / RateNum,

  assert(film(Film, Cat, Dur, NewRate)).
update(_).

% EX10
userAvgRating(User, AverageRating) :-
  vote(User, FilmRatingList), !,
  findall(Rating, member(_Film-Rating, FilmRatingList), Ratings),
  length(Ratings, NumVotes),
  sumlist(Ratings, TotRatings),
  AverageRating is TotRatings / NumVotes.

% este predicado devolve em AverageRating o rating medio que o
% utilizador, User, deu em filmes (nos quais já votou).
% O cut é verde porque quando encontramos o User que queremos
% não necessitamos de fazer backtracking pk sabemos que n existe mais.

% EX11
:-use_module(library(between)).

move(X/Y, Celulas) :-
  getNeighbours(X/Y, Neighbours),
  moveAux(Neighbours, Celulas).
moveAux([X/Y|Neighbours], [X1/Y1|Celulas]):-
  X1 is X, Y1 is Y,
  valid_move(X1/Y1), !,
  moveAux(Neighbours, Celulas).
moveAux([_|Neighbours], Celulas):-
  moveAux(Neighbours, Celulas).
moveAux([], []).
valid_move(X/Y) :- between(1, 8, X), between(1, 8, Y).
getNeighbours(X/Y, [(X-1)/(Y+2), (X-2)/(Y+1), (X+1)/(Y+2), (X+2)/(Y+1), (X-1)/(Y-2),
  (X-2)/(Y-1), (X+1)/(Y-2), (X+2)/(Y-1)]).

% EX12
poderMoverEmNAux(0, Cells, Cells) :- !.
poderMoverEmNAux(N, CellAcc, Cells) :-
    findall(Cells, (member(Move, CellAcc), move(Move, Cells)), NewMoves),
    append(NewMoves, CellList),
    append(CellAcc, CellList, NextCelulas),
    N1 is N - 1,
    poderMoverEmNAux(N1, NextCelulas, Cells).
    
podeMoverEmN(Pos, N, Cells) :-
    poderMoverEmNAux(N, [Pos], Cells2),!,
    sort(Cells2, Cells).
