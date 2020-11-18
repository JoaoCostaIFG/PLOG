% :-use_module(library(random)).

% INPUT FUNCTIONS %
% Using this predicates, the user doesn't need '' around his input
% or . at the end
input(Prompt, Input) :-
  prompt(_, Prompt),
  get_code(Ch),
  once(inputAll(Ch, TodosChars)), % without once it appends inside repeats
  name(Input, TodosChars).

inputNum(Prompt, Input) :-
  input(Prompt, Input),
  number(Input).

inputAll(10, []).
inputAll(13, []).
inputAll(Ch, [Ch | Mais]) :-
  get_code(Ch1),
  inputAll(Ch1, Mais).

% MENU %
menu(GameSettings) :-
  repeat,
    grettingsPanel,
    inputNum('Option: ', Op), parseOp(Op, GameSettings).

grettingsPanel :-
  write('Welcome to emulsion!'), nl,
  write('1 - PvP'), nl,
  write('2 - PvAI'), nl,
  write('3 - AIvP'), nl,
  write('4 - AIvAI'), nl,
  write('0 - Exit'), nl.

% ret = [p1, p2]
% p == 0 => player
% p >= 1 => AI difficulty (1 -> random, 2 -> greedy)
parseOp(1, [0, 0]).
parseOp(2, [0, Dif]) :- getDifficulty(Dif).
parseOp(3, [Dif, 0]) :- getDifficulty(Dif).
parseOp(4, [Dif1, Dif2]) :-
  write('Player 1 difficulty'), nl,
  getDifficulty(Dif1),
  write('Player 2 difficulty'), nl,
  getDifficulty(Dif2).
parseOp(0, _) :- halt(0).

getDifficulty(Difficulty) :-
  repeat,
    write('1 - Random'), nl,
    write('2 - Greedy'), nl,
    inputNum('Difficulty? ', Dif),
    parseDif(Dif, Difficulty).

% parseDif(0, Dif) :- random(1, 3, Dif).
parseDif(1, 1).
parseDif(2, 2).
