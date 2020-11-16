% GAMESTATE OBJECT %
make_state(GameSettings, Board, gameState(GameSettings, Length, Board)) :-
  length(Board, Length).

state_getSettings(gameState(GameSettings, _, _), GameSettings).
state_getPXSettings(gameState([Settings, _], _, _), 0, Settings).
state_getPXSettings(gameState([_, Settings], _, _), 1, Settings).

state_getLength(gameState(_, Length, _), Length).
state_getBoard(gameState(_, _, Board), Board).
state_setBoard(Board, gameState(GameSettings, _, _), gameState(GameSettings, Length, Board)) :-
  length(Board, Length).

state_insideBounds(gameState(_, Length, _), [X, Y]) :-
  X >= 0, X < Length,
  Y >= 0, Y < Length.
state_nth0Board(gameState(_, _, Board), [X, Y], Ret) :-
  nth0_matrix(X, Y, Board, Ret).

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

switch_spots(Matrix, [X, Y, X1, Y1], NewMatrix) :-
  nth0_matrix(X, Y, Matrix, Elem),
  nth0_matrix(X1, Y1, Matrix, Elem1),
  % switch the two spots
  replace_val_matrix(Matrix, Y, X, Elem1, NewMatrix1),
  replace_val_matrix(NewMatrix1, Y1, X1, Elem, NewMatrix).
