:-use_module(library(lists)).

:-include('Emulsion_1_board.pl').
:-include('Emulsion_1_draw.pl').
:-include('Emulsion_1_menu.pl').
:-include('Emulsion_1_state.pl').

play :-
  %menu(GameSettings),
  initial(InitialBoard),
  make_state([0, 0], InitialBoard, InitialState),
  Player is 0,
  write('?').
  %valid_moves(InitialState, Player, ListOfMoves),
  %write(ListOfMoves), halt(0).

game_loop(_, CurrentState, Winner) :-
  game_over(CurrentState, Winner).
game_loop(Player, CurrentState, Winner) :-
  state_getBoard(CurrentState, CurrentBoard),
  display_game(Player, CurrentBoard),
  repeat,
    state_getPXSettings(CurrentState, Player, Dif),
    once(getMove(Dif, Player, CurrentState, Move)),
    once(move(CurrentState, Move, NextState)),
  NextPlayer is mod(Player + 1, 2), % change player
  game_loop(NextPlayer, NextState, Winner).

% GAME LOGIC %
game_over(CurrentState, Winner) :-
  % TODO verify available plays
  fail, % for now, will always fail
  showResult(Winner).

% Player move
getMove(0, Player, CurrentState, Move) :-
  % X & Y
  nl, write('Select a spot of your color.'), nl,
  write('Insert X '), read(X),
  write('Insert Y '), read(Y),
  state_insideBounds(CurrentState, [X, Y]),
  state_nth0Board(CurrentState, [X, Y], Player),
  % Direction
  write('Insert move direction '), read(DirecSymb), nl,
  coordMove([X, Y], DirecSymb, [X1, Y1]),
  state_insideBounds(CurrentState, [X1, Y1]),
  Move = [X, Y, X1, Y1].
% Easy AI
getMove(1, Player, CurrentState, Move) :-
  Move = [0, 0, 1, 0].
% Medium AI
getMove(2, Player, CurrentState, Move) :-
  Move = [0, 0, 1, 0].
% Hard AI
getMove(3, Player, CurrentState, Move) :-
  Move = [0, 0, 1, 0].
% in case of invalid move
getMove(_, _, _) :-
  write('Invalid spot. Try again.'), nl, fail.

% check move and do it
move(GameState, Move, NewGameState) :-
  [X, Y, X1, Y1] = Move,
  state_getBoard(GameState, CurrentBoard),
  once(switch_spots(CurrentBoard, Move, NewBoard)),
  state_setBoard(NewBoard, GameState, NewGameState),
  playValue([X, Y], GameState, CurrV),
  playValue([X1, Y1], NewGameState, NewV),
  NewV > CurrV.
% in case of invalid move
move(_, _, _) :-
  write('Invalid move. Try again.'), nl, fail.

% DIRECTIONS %
direction(0,  -1, 'n').
direction(-1, -1, 'nw').
direction(-1, 0,  'w').
direction(-1, 1,  'sw').
direction(0,  1,  's').
direction(1,  1,  'se').
direction(1,  0,  'e').
direction(1,  -1, 'ne').

% VALUES %
% Base case%
% getValue(X, Y, Value, Last index, [Dirs])
getValue(0, 0, 1, L, ['s', 'e']).
getValue(0, L, 1, L, ['n', 'e']).
getValue(L, 0, 1, L, ['s', 'w']).
getValue(L, L, 1, L, ['n', 'w']).
getValue(X, 0, 0.5, L, ['s', 'e', 'w']).
getValue(X, L, 0.5, L, ['n', 'e', 'w']).
getValue(0, Y, 0.5, L, ['e', 'n', 's']).
getValue(L, Y, 0.5, L, ['w', 'n', 's']).
getValue(_, _, 0, L, ['n', 's', 'e', 'w']).

coordMove([X, Y], Direc, [Xn, Yn]) :-
  direction(X_inc, Y_inc, Direc),
  Xn is X + X_inc,
  Yn is Y + Y_inc.

adjacent(Point, Point, _).
adjacent(Point1, Point2, Directions) :-
  member(Direction, Directions),
  coordMove(Point1, Direction, Point2).

connected([X1, Y1], [X2, Y2], State) :-
  state_getLength(State, L),
  state_insideBounds(State, [X1, Y1]),
  L1 is L - 1, getValue(X1, Y1, _, L1, Direcs),
  adjacent([X1, Y1], [X2, Y2], Direcs),
  state_insideBounds(State, [X2, Y2]),
  state_nth0Board(State, [X1, Y1], Val),
  state_nth0Board(State, [X2, Y2], Val).

getAllAdjacent(Start, Res, _State) :-
  setof(Neighbour, connected(Start, Neighbour, _State), Neighbours),
  searchAdjacent(Neighbours, Res, _State, [], _).

searchAdjacent([], [], _State, Visited, Visited).
searchAdjacent([Neighbour | Neighbours], Res, _State, Visited, NVis) :-
  member(Neighbour, Visited),
  searchAdjacent(Neighbours, Res, _State, Visited, NVis).
searchAdjacent([Neighbour | Neighbours], [Neighbour | Res], _State, Vis, NVis) :-
  \+ member(Neighbour, Vis),
  append(Vis, [Neighbour], TmpVisited),
  setof(NewNeighbour, connected(Neighbour, NewNeighbour, _State), NewNeighbours),
  searchAdjacent(NewNeighbours, CurrRes, _State, TmpVisited, CurrVisited),
  searchAdjacent(Neighbours, NextRes, _State, CurrVisited, NVis),
  append(CurrRes, NextRes, Res).

calcValue([[X, Y] | []], L, V) :-
  L1 is L - 1, getValue(X, Y, V, L1, _).
calcValue([[X, Y] | Coords], L, Res) :-
  L1 is L - 1, getValue(X, Y, V, L1, _),
  % Res = 1 + V + NRes, % tail-recursion but polutes output
  calcValue(Coords, L, NRes),
  Res is 1 + V + NRes.

playValue([X, Y], State, V) :-
  getAllAdjacent([X, Y], Res, State),
  state_getLength(State, L),
  calcValue(Res, L, V).

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


% ListOfMoves : [X1, Y1, X2, Y2] Switch 1 with 2
valid_moves(GameState, Player, ListOfMoves) :-
  setof([X, Y], valid_move(GameState, Player, X, Y), ListOfMoves).
  %exclude(valid_move)

next_player(CurrPlayer, NextPlayer) :-
    NextPlayer is (CurrPlayer + 1) mod 2.

possible_move(GameState, Player, [X1, Y1], [X2, Y2]) :-
    state_getLength(GameState, L),
    state_getBoard(GameState, CurrentBoard),
    getValue(X1, Y1, _, L, Direcs),
    nth0_matrix(X1, Y1, CurrentBoard, Player),
    next_player(Player, NextPlayer),
    nth0_matrix(X2, Y2, CurrentBoard, NextPlayer),
    adjacent([X1, Y1], [X2, Y2], Direcs).

valid_move(GameState, Player, [X1, Y1], [X2, Y2]) :-
    possible_move(GameState, Player, [X1, Y1], [X2, Y2]),

    once(switch_spots(CurrentBoard, [X1, Y1, X2, Y2] , NewBoard)),
    once(state_setBoard(NewBoard, GameState, NewGameState)),
    once(playValue([X1, Y1], GameState, CurrV)),
    once(playValue([X2, Y2], NewGameState, NewV)),
    NewV > CurrV.
