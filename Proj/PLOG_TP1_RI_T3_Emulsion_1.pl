:-use_module(library(lists)).

:-include('Emulsion_1_board.pl').
:-include('Emulsion_1_draw.pl').

play :-
  initial(InitialState),
  assert(state(0, InitialState)),
  repeat,
    retract(state(Player, CurrentState)),
    display_game(Player, CurrentState),
    once(mkMove(Player, CurrentState, NextPlayer, NextState)),
    assert(state(NextPlayer, NextState)),
    game_over(NextState, Winner),
  showResult(Winner).

game_over(_, _) :- fail. % for now, will always fail

% GAME LOGIC %
mkMove(Player, CurrentBoard, NextPlayer, NextBoard) :-
  getMove(X, Y, DirecSymb),
  direction(DirecX, DirecY, DirecSymb),
  X1 is X + DirecX,
  Y1 is Y + DirecY,
  % TODO check if move is valid (group value + diferent colors)
  % switch the two spots
  nth0_matrix(X, Y, CurrentBoard, Elem),
  nth0_matrix(X1, Y1, CurrentBoard, Elem1),
  replace_val_matrix(CurrentBoard, Y, X, Elem1, NextBoard1),
  replace_val_matrix(NextBoard1, Y1, X1, Elem, NextBoard),
  % change to the next player
  NextPlayer is mod(Player + 1, 2).

getMove(X, Y, DirecSymb) :-
  write('Insert X '), read(X),
  write('Insert Y '), read(Y),
  write('Insert move direction '), read(DirecSymb),
  nl.

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

% DIRECTIONS %
direction(0,  -1, 'n').
direction(-1, -1, 'nw').
direction(-1, 0,  'w').
direction(-1, 1,  'sw').
direction(0,  1,  's').
direction(1,  1,  'se').
direction(1,  0,  'e').
direction(1,  -1, 'ne').
