:-use_module(library(lists)).

:-include('Emulsion_1_board.pl').
:-include('Emulsion_1_draw.pl').

play :-
  initial(InitialState),
  Player is 0,
  game_loop(Player, InitialState, Winner).

game_loop(Player, CurrentState, Winner) :-
  game_over(CurrentState, Winner).
game_loop(Player, CurrentState, Winner) :-
  display_game(Player, CurrentState),
  repeat,
    getMove(Player, CurrentState, Move),
    move(CurrentState, Move, NextState),
  NextPlayer is mod(Player + 1, 2), % change player
  game_loop(NextPlayer, NextState, Winner).

% GAME LOGIC %
game_over(CurrentState, Winner) :-
  % TODO verify available plays
  fail, % for now, will always fail
  showResult(Winner).

getMove(Player, CurrentState, Move) :-
  % X & Y
  write('Select a spot of your color.'), nl,
  write('Insert X '), read(X),
  write('Insert Y '), read(Y),
  X > -1, Y > -1, % TODO verify upper limit
  nth0_matrix(X, Y, CurrentState, Color), Player = Color,
  % Direction
  write('Insert move direction '), read(DirecSymb),
  nl,
  direction(DirecX, DirecY, DirecSymb),
  X1 is X + DirecX,
  Y1 is Y + DirecY,
  X1 > -1, Y1 > -1, % TODO verify upper limit
  Move = [X, Y, X1, Y1].
% in case of invalid move
getMove(_, _, _) :-
  write('Invalid move. Try again.'), nl, fail.

move(GameState, Move, NewGameState) :-
  switch_spots(GameState, Move, NewGameState).
% in case of invalid move
move(GameState, Move, NewGameState) :-
  write('Invalid move. Try again.'), nl, fail.

switch_spots(CurrentState, [X, Y, X1, Y1], NextState) :-
  switch_spots(CurrentState, X, Y, X1, Y1, NextState).
switch_spots(CurrentState, X, Y, X1, Y1, NextState) :-
  nth0_matrix(X, Y, CurrentState, Elem),
  nth0_matrix(X1, Y1, CurrentState, Elem1),
  % switch the two spots
  replace_val_matrix(CurrentState, Y, X, Elem1, NextState1),
  replace_val_matrix(NextState1, Y1, X1, Elem, NextState).

% DIRECTIONS %
direction(0,  -1, 'n').
direction(-1, -1, 'nw').
direction(-1, 0,  'w').
direction(-1, 1,  'sw').
direction(0,  1,  's').
direction(1,  1,  'se').
direction(1,  0,  'e').
direction(1,  -1, 'ne').

% MATRIX MANIPULATION %
replace_val([_|T], 0, X, [X|T]).
replace_val([H|T], I, X, [H|R]) :-
  I > -1,
  NI is I - 1,
  replace_val(T, NI, X, R), !.
replace_val(L, _, _, L).

replace_val_matrix([H|T], 0, Col, X, [R|T]) :-
  replace_val(H, Col, X, R).
replace_val_matrix([H|T], Line, Col, X, [H|R]) :-
  Line > -1,
  Line1 is Line - 1,
  replace_val_matrix(T, Line1, Col, X, R).

nth0_matrix(X, Y, Matrix, Elem) :-
  nth0(Y, Matrix, List),
  nth0(X, List, Elem).
