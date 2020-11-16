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
