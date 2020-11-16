:-use_module(library(random)).

% INPUT FUNCTIONS %
input(Input) :-
  get_code(Ch),
  inputAll(Ch, TodosChars),
  name(Input, TodosChars).

inputAll(10, []).
inputAll(13, []).
inputAll(Ch, [Ch | Mais]) :-
  get_code(Ch1),
  inputAll(Ch1, Mais).

% MENU %
menu(GameSettings) :-
  repeat,
    grettingsPanel,
    input(Op), parseOp(Op, GameSettings).

grettingsPanel :-
  write('Welcome to emulsion!'), nl,
  write('1 - PvP'), nl,
  write('2 - PvAI'), nl,
  write('3 - AIvAI'), nl,
  write('0 - Exit'), nl.

% ret = [p1, p2]
% p == 0 => player
% p >= 1 => AI difficulty (1 -> easy, 2 -> medium, 3 -> hard)
parseOp(1, [0, 0]).
parseOp(2, [0, Dif]) :- getDifficulty(Dif).
parseOp(3, [Dif1, Dif2]) :-
  write('Player 1 difficulty'), nl,
  getDifficulty(Dif1),
  write('Player 2 difficulty'), nl,
  getDifficulty(Dif2).
parseOp(0, _) :- halt(0).

getDifficulty(Difficulty) :-
  write('1 - Easy'), nl,
  write('2 - Medium'), nl,
  write('3 - Hard'), nl,
  write('0 - Random'), nl,
  write('Difficulty? '), input(Dif),
  parseDif(Dif, Difficulty).

parseDif(0, Dif) :- random(1, 3, Dif).
parseDif(Dif, Dif).
