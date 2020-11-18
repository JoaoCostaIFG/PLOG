:-use_module(library(lists)).

:-include('Emulsion_1_board.pl').
:-include('Emulsion_1_draw.pl').
:-include('Emulsion_1_menu.pl').
:-include('Emulsion_1_state.pl').

play :-
  menu(GameSettings),
  initial(InitialState1),
  state_setSettings(GameSettings, InitialState1, InitialState),
  state_getPlayer(InitialState, Player),
  game_loop(Player, InitialState),
  play. % not sure about this recursion

game_loop(Player, CurrentState) :-
  state_getBoard(CurrentState, CurrentBoard),
  display_game(Player, CurrentBoard),
  game_over(CurrentState, _Winner).
game_loop(Player, CurrentState) :-
  repeat,
    state_getPXSettings(CurrentState, Player, Dif),
    once(choose_move(CurrentState, Player, Dif, Move)),
    once(move(CurrentState, Move, StateAfterMove)),
  state_nextPlayer(StateAfterMove, NextPlayer, NextState), % change player
  game_loop(NextPlayer, NextState).

% GAME LOGIC %
game_over(GameState, Winner) :-
  state_getPlayer(GameState, Player),
  write('checking '), write(Player), nl,
  \+valid_moves(GameState, Player, _),
  write('no more '), write(Player), nl,
  value(GameState, 0, VL0), value(GameState, 1, VL1),
  parseValueList(VL0, VL1, V0, V1, Winner),
  showResult(V0, V1, Winner).

% Player move
choose_move(GameState, Player, 0, Move) :-
  % X & Y
  nl, write('Select a spot of your color.'), nl,
  inputNum('X? ', X), inputNum('Y? ', Y),
  state_insideBounds(CurrentState, [X, Y]),
  state_nth0Board(CurrentState, [X, Y], Player),
  % Direction
  input('Move direction? ', DirecSymb), nl,
  coordMove([X, Y], DirecSymb, [X1, Y1]),
  state_insideBounds(CurrentState, [X1, Y1]),
  Move = [X, Y, X1, Y1].
% Easy AI
choose_move(GameState, Player, 1, Move) :-
  Move = [0, 0, 1, 0].
% Medium AI
choose_move(GameState, Player, 2, Move) :-
  Move = [0, 0, 1, 0].
% Hard AI
choose_move(GameState, Player, 3, Move) :-
  Move = [0, 0, 1, 0].
% in case of invalid move (inputed by the user)
choose_move(_, _, _, _) :-
  write('Invalid spot. Try again.'), nl, fail.

% check move and do it
move(GameState, Move, NewGameState) :-
  [X, Y, X1, Y1] = Move,
  state_getBoard(GameState, CurrentBoard),
  once(switch_spots(CurrentBoard, Move, NewBoard)),
  state_setBoard(NewBoard, GameState, NewGameState),
  once(playValue([X, Y], GameState, CurrV)),
  once(playValue([X1, Y1], NewGameState, NewV)),
  NewV > CurrV.
% in case of invalid move
move(_, _, _) :-
  write('Invalid move. Try again.'), nl, fail.

% DIRECTIONS %
direction(0,    -1, 'n').
direction(-1,   -1, 'nw').
direction(-1,   0,  'w').
direction(-1,   1,  'sw').
direction(0,    1,  's').
direction(1,    1,  'se').
direction(1,    0,  'e').
direction(1,    -1, 'ne').

% VALUES %
% Base case%
% getValue(X, Y,    Value, Last index, [Dirs])
getValue(0,   0,    1,    _L, ['s', 'e']).
getValue(0,   _L,   1,    _L, ['n', 'e']).
getValue(_L,  0,    1,    _L, ['s', 'w']).
getValue(_L,  _L,   1,    _L, ['n', 'w']).
getValue(_X,  0,    0.5,  _L, ['s', 'e', 'w']).
getValue(_X,  _L,   0.5,  _L, ['n', 'e', 'w']).
getValue(0,   _Y,   0.5,  _L, ['e', 'n', 's']).
getValue(_L,  _Y,   0.5,  _L, ['w', 'n', 's']).
getValue(_,   _,    0,    _L, ['n', 's', 'e', 'w']).

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
  Res = 1 + V + NRes,
  calcValue(Coords, L, NRes).

playValue([X, Y], State, V) :-
  getAllAdjacent([X, Y], Res, State),
  state_getLength(State, L),
  calcValue(Res, L, V).

getAllGroups(_State, _Player, [], [], _Visited).
getAllGroups(State, Player, [G|Groups], [C|Coords], Visited) :-
  \+member(C, Visited),
  getAllAdjacent(C, G, State),
  append(Visited, G, NewVisited),
  getAllGroups(State, Player, Groups, Coords, NewVisited).
getAllGroups(State, Player, Groups, [_C|Coords], Visited) :- % pop C
  getAllGroups(State, Player, Groups, Coords, Visited).
getAllGroups(State, Player, Groups) :-
  bagof(C, state_nth0Board(State, C, Player), CoordList),
  getAllGroups(State, Player, Groups, CoordList, []).

getAllGroupsValues(_, [], []).
getAllGroupsValues(State, [G|Groups], [R|Res]) :-
  length(G, R),
  getAllGroupsValues(State, Groups, Res).

% Returns sorted (desc.) list of the values of all groups
value(GameState, Player, Value) :-
  getAllGroups(GameState, Player, G),
  getAllGroupsValues(GameState, G, ListOfVals),
  sort(ListOfVals, SortedVals), reverse(SortedVals, Value).

% Receives 2 lists of group sizes (calculated by value())
% Returns the 2 game results (stops summing when a winner is found
% or a draw is decided). Winner will store a number representing the
% end result 2 - Tie; 1 - Player 1 Wins; 2- Player 2 Wins
parseValueList(VL0, VL1, V1P, V2P, Winner) :-
    parseValueListN(VL0, VL1, Winner, V1P, Dif),
    V2P is V1P + Dif.
parseValueListN([], [], 2, 0, 0). % Tie
parseValueListN([V0 | VL0], [V1 | VL1], 0, V0, Dif) :- % Player 0 Wins
    V0 > V1,
    Dif is V1 - V0.
parseValueListN([V0 | VL0], [V1 | VL1], 1, V0, Dif) :- % Player 1 Wins
    V1 > V0,
    Dif is V1 - V0.
parseValueListN([V | VL0], [V | VL1], Winner, NAcc, Dif) :- 
    parseValueListN(VL0, VL1, Winner, Acc, Dif),
    NAcc is Acc + V.

% ListOfMoves : [X1, Y1, X2, Y2] Switch 1 with 2
valid_moves(GameState, Player, ListOfMoves) :-
  setof([X, Y], valid_move(GameState, Player, X, Y), ListOfMoves).

possible_move(GameState, Player, [X1, Y1], [X2, Y2]) :-
  state_getLength(GameState, L),
  getValue(X1, Y1, _, L, Direcs),
  next_player(Player, NextPlayer),
  state_nth0Board(GameState, [X1, Y1], Player),
  state_nth0Board(GameState, [X2, Y2], NextPlayer),
  adjacent([X1, Y1], [X2, Y2], Direcs).

valid_move(GameState, Player, [X1, Y1], [X2, Y2]) :-
  possible_move(GameState, Player, [X1, Y1], [X2, Y2]),
  state_getBoard(GameState, CurrentBoard),
  once(switch_spots(CurrentBoard, [X1, Y1, X2, Y2] , NewBoard)),
  state_setBoard(NewBoard, GameState, NewGameState),
  once(playValue([X1, Y1], GameState, CurrV)),
  once(playValue([X2, Y2], NewGameState, NewV)),
  NewV > CurrV.
