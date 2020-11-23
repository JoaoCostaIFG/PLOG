%%%%%%%%%%%%
% EMULSION %
%%%%%%%%%%%%

:-use_module(library(system)).
:-use_module(library(random)).

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
  display_game(CurrentState, Player),
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
  \+valid_moves(GameState, Player, _),
  value(GameState, 0, VL0), value(GameState, 1, VL1),
  parseValueList(VL0, VL1, V0, V1, Winner),
  show_result(V0, V1, Winner, Player).

% Player move
choose_move(GameState, Player, 0, Move) :-
  % X & Y
  nl, write('Select a spot of your color.'), nl,
  inputNum('X? ', X), inputNum('Y? ', Y),
  state_insideBounds(GameState, [X, Y]),
  state_nth0Board(GameState, [X, Y], Player),
  % Direction
  input('Move direction? ', DirecSymb), nl,
  coordMove([X, Y], DirecSymb, [X1, Y1]),
  state_insideBounds(GameState, [X1, Y1]),
  Move = [[X, Y], [X1, Y1]].
% Easy AI
choose_move(GameState, Player, 1, Move) :-
  valid_moves(GameState, Player, Moves),
  ai_getBestMove(GameState, Player, Moves, 1, Move, _),
  ai_moveAnnounce('Easy', Move).
% Medium AI
choose_move(GameState, Player, 2, Move) :-
  valid_moves(GameState, Player, Moves),
  ai_getBestMove(GameState, Player, Moves, 2, Move, _),
  ai_moveAnnounce('Medium', Move).
% Hard AI
choose_move(GameState, Player, 3, Move) :-
  valid_moves(GameState, Player, Moves),
  ai_getBestMove(GameState, Player, Moves, 3, Move, _),
  ai_moveAnnounce('SCIENTIA', Move).
% Random play AI
choose_move(GameState, Player, 4, Move) :-
  valid_moves(GameState, Player, Moves),
  length(Moves, L), random(0, L, RdmInd),
  nth0(RdmInd, Moves, Move),
  ai_moveAnnounce('Random move', Move).
% in case of invalid move (inputed by the user)
choose_move(_, _, _, _) :-
  write('Invalid spot. Try again.'), nl, fail.

ai_moveAnnounce(AILevel, [P1, P2]) :-
  nl,
  format('~w AI making move:', [AILevel]), nl,
  format(' - from: ~w', [P1]), nl,
  format(' - to: ~w', [P2]), nl,
  nl,
  sleep(2).
  
ai_getBestMoveChoose(0, Move0, _, VL0, _, Move0, VL0).
ai_getBestMoveChoose(1, _, Move1, _, VL1, Move1, VL1).
ai_getBestMoveChoose(2, Move0, _, VL0, _, Move0, VL0).

ai_getBestMoveNoValidMoves(GameState, AI_Player, [0]) :-
  next_player(AI_Player, NotAI_Player),
  value(GameState, AI_Player, VL0),
  value(GameState, NotAI_Player, VL1),
  parseValueList(VL0, VL1, _, _, NotAI_Player).
  %write(AI_Player), write(Winner),
  %write('MOVE BAD:(:(.'), write(Move), write('\n').
% TODO 999999?
ai_getBestMoveNoValidMoves(GameState, AI_Player, [99999]) :-
  next_player(AI_Player, NotAI_Player),
  value(GameState, AI_Player, VL0),
  value(GameState, NotAI_Player, VL1),
  parseValueList(VL0, VL1, _, _, AI_Player).
  %write(AI_Player), write(Winner),
  %write('MOVE GUD!!!!'), write(Move), write('\n'),

ai_getBestMove(_, _, [], _, _, []).
ai_getBestMove(GameState, Player, [Move|Moves], 1, BestMove, Val) :-
  move(GameState, Move, NewGameState),
  value(NewGameState, Player, VL0),
  ai_getBestMove(GameState, Player, Moves, 1, Move1, VL1),
  parseValueList(VL0, VL1, _, _, Winner),
  ai_getBestMoveChoose(Winner, Move, Move1, VL0, VL1, BestMove, Val).
ai_getBestMove(GameState, Player, [Move|Moves], Level, BestMove, Val) :-
  % Do one of valid moves
  move(GameState, Move, NewGameState),
  % Choose best choice for enemy
  next_player(Player, NPlayer),
  once(valid_moves(NewGameState, NPlayer, EnemyMoves)),
  ai_getBestMove(NewGameState, NPlayer, EnemyMoves, 1, BestEnemyMove, _),
  % Play that choice
  move(NewGameState, BestEnemyMove, NewEnemyGameState),
  % get best move level - 1
  valid_moves(NewEnemyGameState, Player, NewMoves),
  NLevel is Level - 1,
  ai_getBestMove(NewEnemyGameState, Player, NewMoves, NLevel, _, VL0),
  % do rest of valid moves
  ai_getBestMove(GameState, Player, Moves, Level, BestMove1, VL1),
  % See if valid move has more value
  parseValueList(VL0, VL1, _, _, Winner),
  ai_getBestMoveChoose(Winner, Move, BestMove1, VL0, VL1, BestMove, Val).
ai_getBestMove(GameState, Player, [Move|Moves], Level, BestMove, Val) :-
  valid_move(GameState, Move, NewGameState),
  % Enemy has no moves
  next_player(Player, NPlayer),
  \+ once(valid_moves(NewGameState, NPlayer, _)),
  ai_getBestMoveNoValidMoves(NewGameState, Player, VL0),

  ai_getBestMove(GameState, Player, Moves, Level, Move1, VL1),
  parseValueList(VL0, VL1, _, _, Winner),
  ai_getBestMoveChoose(Winner, Move, Move1, VL0, VL1, BestMove, Val).
ai_getBestMove(GameState, Player, [Move|Moves], Level, BestMove, Val) :- % TODO
  valid_move(GameState, Move, NewGameState),
  % Enemy has no moves
  next_player(Player, NPlayer),
  once(valid_moves(NewGameState, NPlayer, EnemyMoves)),
  once(ai_getBestMove(NewGameState, NPlayer, EnemyMoves, 1, BestEnemyMove, _)),
  valid_move(NewGameState, BestEnemyMove, NewEnemyGameState),

  \+ once(valid_moves(NewEnemyGameState, Player, _)),
  ai_getBestMoveNoValidMoves(NewEnemyGameState, Player, VL0),
  ai_getBestMove(GameState, Player, Moves, Level, Move1, VL1),
  parseValueList(VL0, VL1, _, _, Winner),
  ai_getBestMoveChoose(Winner, Move, Move1, VL0, VL1, BestMove, Val).

% check move and do it
move(GameState, Move, NewGameState) :-
  % we have a wrapper for this predicate that offers more
  % user interation/friendliness. valid_move is used to calculate
  % all the available moves
  valid_move(GameState, Move, NewGameState).
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
% getValue([X, Y],  Value, Last index, [Dirs])
getValue([0,  0 ],  1,    _L, ['s', 'e', 'se']).
getValue([0,  _L],  1,    _L, ['n', 'e', 'ne']).
getValue([_L, 0 ],  1,    _L, ['s', 'w', 'sw']).
getValue([_L, _L],  1,    _L, ['n', 'w', 'nw']).
getValue([_X, 0 ],  0.5,  _L, ['s', 'e', 'w', 'se', 'sw']).
getValue([_X, _L],  0.5,  _L, ['n', 'e', 'w', 'ne', 'nw']).
getValue([0,  _Y],  0.5,  _L, ['e', 'n', 's', 'ne', 'ne']).
getValue([_L, _Y],  0.5,  _L, ['w', 'n', 's', 'nw', 'sw']).
getValue([_,  _ ],  0,    _L, ['n', 's', 'e', 'w', 'ne', 'se', 'nw', 'sw']).

coordMove([X, Y], Direc, [Xn, Yn]) :-
  direction(X_inc, Y_inc, Direc),
  Xn is X + X_inc,
  Yn is Y + Y_inc.

adjacent(Point, Point, _).
adjacent(Point1, Point2, Directions) :-
  member(Direction, Directions),
  coordMove(Point1, Direction, Point2).

% conDir([X, Y],  Last index, [Dirs])
conDir([0,  0 ],    _L,         ['s', 'e']).
conDir([0,  _L],    _L,         ['n', 'e']).
conDir([_L, 0 ],    _L,         ['s', 'w']).
conDir([_L, _L],    _L,         ['n', 'w']).
conDir([_X, 0 ],    _L,         ['s', 'e', 'w']).
conDir([_X, _L],    _L,         ['n', 'e', 'w']).
conDir([0,  _Y],    _L,         ['e', 'n', 's']).
conDir([_L, _Y],    _L,         ['w', 'n', 's']).
conDir([_,  _ ],    _L,         ['n', 's', 'e', 'w']).

connected(P1, P2, State) :-
  state_getLength(State, L),
  state_insideBounds(State, P1),
  L1 is L - 1, conDir(P1, L1, Direcs),
  adjacent(P1, P2, Direcs),
  state_insideBounds(State, P2),
  state_nth0Board(State, P1, Val),
  state_nth0Board(State, P2, Val).

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

pieceValue([X, Y], State, V) :-
  setof(Neighbour, connected([X, Y], Neighbour, State), Neighbours),
  state_getLength(State, L), L1 is L - 1,
  once(getValue([X, Y], PieceVal, L1, _)),
  length(Neighbours, NeighbNum), % Neighbours includes [X, Y] (do - 1)
  V is (NeighbNum - 1) + PieceVal.

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

% Receives 2 lists of group sizes (calculated by the value predicate)
% Returns the 2 final scores (one for each player)
% Winner will store a number representing the
% end  0 - Player 0 Wins; 1 - Player 1 Wins; result 2 - Tie;
parseValueList(VL0, VL1, Value0, Value1, Winner) :-
  parseValueListN(VL0, VL1, Value0, Value1, 0, 0),
  valueCmp(Value0, Value1, Winner).
parseValueListN([], [], Acc0, Acc1, Acc0, Acc1) :- !.
parseValueListN([], [V1|VL1], Value0, Value1, Acc, Acc) :-
  NewAcc1 is Acc + V1,
  parseValueListN([], VL1, Value0, Value1, Acc, NewAcc1).
parseValueListN([V0|VL0], [], Value0, Value1, Acc, Acc) :-
  NewAcc0 is Acc + V0,
  parseValueListN(VL0, [], Value0, Value1, NewAcc0, Acc).
parseValueListN([V0|VL0], [V1|VL1], Value0, Value1, Acc, Acc) :-
  NewAcc0 is Acc + V0,
  NewAcc1 is Acc + V1,
  parseValueListN(VL0, VL1, Value0, Value1, NewAcc0, NewAcc1).
parseValueListN(_, _, Acc0, Acc1, Acc0, Acc1) :- Acc0 \= Acc1.

% returns 0, if V0 > V1
% returns 1, if V0 < V1
% returns 2, if V0 = V1
valueCmp(V0, V1, 0) :- V0 > V1.
valueCmp(V0, V1, 1) :- V0 < V1.
valueCmp(V0, V1, 2) :- V0 = V1.

% ListOfMoves : [X1, Y1, X2, Y2] Switch 1 with 2
valid_moves(GameState, Player, ListOfMoves) :-
  setof(Move, valid_move_full(GameState, Player, Move), ListOfMoves).

valid_move_full(GameState, Player, [P1, P2]) :-
  state_getLength(GameState, L),
  getValue(P1, _, L, Direcs),
  next_player(Player, NextPlayer),
  state_nth0Board(GameState, P1, Player),
  state_nth0Board(GameState, P2, NextPlayer),
  adjacent(P1, P2, Direcs),
  valid_move(GameState, [P1, P2], _).

valid_move(GameState, [P1, P2], NewGameState) :-
  state_getBoard(GameState, CurrentBoard),
  once(switch_spots(CurrentBoard, [P1, P2] , NewBoard)),
  state_setBoard(NewBoard, GameState, NewGameState),
  pieceValue(P1, GameState, CurrV),
  pieceValue(P2, NewGameState, NewV),
  NewV > CurrV.

