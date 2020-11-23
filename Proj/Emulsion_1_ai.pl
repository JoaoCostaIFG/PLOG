%%%%%%%%%%%%%%%%
% AI FUNCTIONS %
%%%%%%%%%%%%%%%%

:-use_module(library(random)).

%
%
% TODO para n me esquecer. O bloco interno (depois do valid_moves)
% devia ser separado
%
%

% show the move the AI will make to the player
ai_moveAnnounce(AILevel, [P1, P2]) :-
  nl,
  format('~w AI making move:', [AILevel]), nl,
  format(' - from: ~w', [P1]), nl,
  format(' - to: ~w', [P2]), nl,
  nl,
  sleep(2).

% choose the best move (more value) between 2 moves
ai_getBestMoveChoose(0, Move0, _, VL0, _, Move0, VL0).
ai_getBestMoveChoose(1, _, Move1, _, VL1, Move1, VL1).
ai_getBestMoveChoose(2, Move0, Move1, VL0, VL1, Move, VL) :-
  % if 2 choices with the same value come up, we choose a random one
  random(0, 2, Rdm),
  ai_getBestMoveChoose(Rdm, Move0, Move1, VL0, VL1, Move, VL).

ai_getBestMoveNoValidMoves(GameState, AI_Player, [0]) :-
  next_player(AI_Player, NotAI_Player),
  value(GameState, AI_Player, VL0),
  value(GameState, NotAI_Player, VL1),
  parseValueList(VL0, VL1, _, _, NotAI_Player).
ai_getBestMoveNoValidMoves(GameState, AI_Player, [MaxPoints]) :-
  state_getLength(GameState, L), MaxPoints is L * L, % no player can reach this amount points
  next_player(AI_Player, NotAI_Player),
  value(GameState, AI_Player, VL0),
  value(GameState, NotAI_Player, VL1),
  parseValueList(VL0, VL1, _, _, AI_Player).

ai_getBestMove(_, _, [], _, _, []).
ai_getBestMove(GameState, Player, [Move|Moves], 1, BestMove, Val) :-
  valid_move(GameState, Move, NewGameState),
  value(NewGameState, Player, VL0),
  ai_getBestMove(GameState, Player, Moves, 1, Move1, VL1),
  parseValueList(VL0, VL1, _, _, Winner),
  ai_getBestMoveChoose(Winner, Move, Move1, VL0, VL1, BestMove, Val).

% TODO por em baixo
ai_getBestMove(GameState, Player, [Move|Moves], Level, BestMove, Val) :-
  valid_move(GameState, Move, NewGameState),
  next_player(Player, NPlayer),
  \+ once(valid_moves(NewGameState, NPlayer, _)), % Enemy has no moves
  ai_getBestMoveNoValidMoves(NewGameState, Player, VL0),

  ai_getBestMove(GameState, Player, Moves, Level, Move1, VL1),
  parseValueList(VL0, VL1, _, _, Winner),
  ai_getBestMoveChoose(Winner, Move, Move1, VL0, VL1, BestMove, Val).
ai_getBestMove(GameState, Player, [Move|Moves], Level, BestMove, Val) :- % TODO
  valid_move(GameState, Move, NewGameState),
  next_player(Player, NPlayer),
  once(valid_moves(NewGameState, NPlayer, EnemyMoves)), % Enemy has no moves
  once(ai_getBestMove(NewGameState, NPlayer, EnemyMoves, 1, BestEnemyMove, _)),
  valid_move(NewGameState, BestEnemyMove, NewEnemyGameState),

  \+ once(valid_moves(NewEnemyGameState, Player, _)),
  ai_getBestMoveNoValidMoves(NewEnemyGameState, Player, VL0),
  ai_getBestMove(GameState, Player, Moves, Level, Move1, VL1),
  parseValueList(VL0, VL1, _, _, Winner),
  ai_getBestMoveChoose(Winner, Move, Move1, VL0, VL1, BestMove, Val).

ai_getBestMove(GameState, Player, [Move|Moves], Level, BestMove, Val) :-
  % execute 1 move
  valid_move(GameState, Move, NewGameState),
  % execute best move for enemy player based on our move
  next_player(Player, NPlayer),
  once(valid_moves(NewGameState, NPlayer, EnemyMoves)),
  ai_getBestMove(NewGameState, NPlayer, EnemyMoves, 1, BestEnemyMove, _),
  valid_move(NewGameState, BestEnemyMove, NewEnemyGameState),

  % evalute next level (level - 1)
  valid_moves(NewEnemyGameState, Player, NewMoves),
  NLevel is Level - 1,
  ai_getBestMove(NewEnemyGameState, Player, NewMoves, NLevel, _, VL0),

  % execute rest of valid moves
  ai_getBestMove(GameState, Player, Moves, Level, BestMove1, VL1),
  % return the most valueable move
  parseValueList(VL0, VL1, _, _, Winner),
  ai_getBestMoveChoose(Winner, Move, BestMove1, VL0, VL1, BestMove, Val).
