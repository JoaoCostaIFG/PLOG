playIO :-
  initial(-GameState),
  Player is 0,
  display_game(+GameState, +Player).

% DRAWING %

% draws a given board/GameState on the console
display_game(+GameState, +Player) :-
  print_board(GameState).

print_board([]).
print_board([L | B]) :-
  length(L, Len),
  print_line_top(Len), nl,
  print_line(L), nl,
  print_line_bot(Len), nl,
  print_board(B).

% ┌───┐
% │ W │
% └───┘
% prints the top of boxes (cels) of a given line
% N is the number of cells on the line
print_line_top(0).
print_line_top(N) :-
  N > 0,
  put_code(9484), put_code(9472), put_code(9472), put_code(9472), put_code(9488),
  N1 is N - 1, print_line_top(N1).

% prints the bottom of boxes (cels) of a given line
% N is the number of cells on the line
print_line_bot(0).
print_line_bot(N) :-
  N > 0,
  put_code(9492), put_code(9472), put_code(9472), put_code(9472), put_code(9496),
  N1 is N - 1, print_line_bot(N1).

% prints the center of the boxes (cells) and the cell content of a given line
print_line([]).
print_line([C | L]) :-
  print_cell(C),
  print_line(L).

print_cell(C) :-
  cell_code(C, S),
  put_code(9474), write('\33\[1m\33\[34m'), write(S), write('\33\[0m'), put_code(9474).

% Get 15x15 initial board (checkered)
initial(-GameState) :-
  GameState = [
    [1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1],
    [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0],
    [1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1],
    [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0],
    [1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1],
    [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0],
    [1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1],
    [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0],
    [1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1],
    [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0],
    [1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1],
    [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0],
    [1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1],
    [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0],
    [1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
  ].

cell_code(0, '\33\[40m B ').
cell_code(1, '\33\[47m W ').

% GAME LOGIC %

% switch_places(+GameState, -NewState, +X1, +Y1, +X2, +Y2) :- .

