:-use_module(library(lists)).

:-include('Emulsion_1_board.pl').
:-include('Emulsion_1_draw.pl').
:-include('Emulsion_1_menu.pl').
:-include('Emulsion_1_state.pl').

play :-
  menu(GameSettings),
  initial(InitialBoard),
  make_state(GameSettings, InitialBoard, InitialState),
  Player is 0,
  game_loop(Player, InitialState, Winner).

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
  write('Insert X '), input(X),
  write('Insert Y '), input(Y),
  state_insideBounds(CurrentState, [X, Y]),
  state_nth0Board(CurrentState, [X, Y], Player),
  % Direction
  write('Insert move direction '), input(DirecSymb), nl,
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

coordMove([X, Y], Direc, [Xn, Yn]) :-
  direction(X_inc, Y_inc, Direc),
  Xn is X + X_inc,
  Yn is Y + Y_inc.

% VALUES %
% Base case %
% getValue(X, Y, Value, Last index, [Dirs])
getValue(0,   0,  1,    _, ['s', 'e']) :- !.
getValue(0,   _L, 1,    _L, ['n', 'e']) :- !.
getValue(_L,  0,  1,    _L, ['s', 'w']) :- !.
getValue(_L,  _L, 1,    _L, ['n', 'w']) :- !.
getValue(_X,  0,  0.5,  _L, ['s', 'e', 'w']) :- !.
getValue(_X,  _L, 0.5,  _L, ['n', 'e', 'w']) :- !.
getValue(0,   _Y, 0.5,  _L, ['e', 'n', 's']) :- !.
getValue(_L,  _Y, 0.5,  _L, ['w', 'n', 's']) :- !.
getValue(_,   _,  0,    _L, ['n', 's', 'e', 'w']) :- !.

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
  findall(Neighbour, connected(Start, Neighbour, _State), Neighbours),
  searchAdjacent(Neighbours, Res, _State, [], _).

searchAdjacent([], [], _State, Visited, Visited).
searchAdjacent([Neighbour | Neighbours], Res, _State, Visited, NVis) :-
  member(Neighbour, Visited),
  searchAdjacent(Neighbours, Res, _State, Visited, NVis).
searchAdjacent([Neighbour | Neighbours], [Neighbour | Res], _State, Vis, NVis) :-
  \+ member(Neighbour, Vis),
  append(Vis, [Neighbour], TmpVisited),
  findall(NewNeighbour, connected(Neighbour, NewNeighbour, _State), NewNeighbours),
  searchAdjacent(NewNeighbours, CurrRes, _State, TmpVisited, CurrVisited),
  searchAdjacent(Neighbours, NextRes, _State, CurrVisited, NVis),
  append(CurrRes, NextRes, Res).

calcValue([[X, Y] | []], L, V) :-
  L1 is L - 1, getValue(X, Y, V, L1, _).
calcValue([[X, Y] | Coords], L, Res) :-
  L1 is L - 1, getValue(X, Y, V, L1, _),
  Res = 1 + V + NRes,
  calcValue(Coords, L, NRes).

playValue([X, Y], State, V) :-
  getAllAdjacent([X, Y], Res, State),
  state_getLength(State, L),
  calcValue(Res, L, V).

