%%%%%%%%%%%%%%%%
% AI FUNCTIONS %
%%%%%%%%%%%%%%%%

:-use_module(library(random)).

%
%
% TODO para n me esquecer. O bloco interno (depois do valid_moves)
% devia ser separado
%
% Usar move random para enemy em vez de best move
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

% ALPHA BETA

ai_getBestMove(GameState, Player, Moves, Depth, Move) :-
  alphabeta(GameState, Moves, Player, -1, 9999, Depth, 1, Move).

alphabeta(GameState, Moves, Player, Alpha, Beta, 0, Maximizing, Move) :-
  value(GameState, Player)
alphabeta(GameState, Moves, Player, Alpha, Beta, Depth, Maximizing, Move) :-

