play :-
  initial(-GameState),
  Player is 0,
  display_game(+GameState, +Player).

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

% DRAWING %

% draws a given board/GameState on the console
display_game(+GameState, +Player) :-
  print_board(GameState).

print_board([]).
print_board([L | B]) :-
  print_line(L),
  print_board(B).

% ┌───┐
% │ W │
% └───┘
print_line([]).
print_line(L) :-
  print_line_top(L), nl,
  print_line_center(L), nl,
  print_line_bot(L), nl.

% prints the top of boxes (cels) of a given line
print_line_top([]).
print_line_top([C | L]) :-
  set_bg_color(+C),
  set_fg_color(+C),
  put_code(9484), put_code(9472), put_code(9472), put_code(9472), put_code(9488),
  reset_ansi,
  print_line_top(L).

% prints the bottom of boxes (cels) of a given line
print_line_bot([]).
print_line_bot([C | L]) :-
  set_bg_color(+C),
  set_fg_color(+C),
  put_code(9492), put_code(9472), put_code(9472), put_code(9472), put_code(9496),
  reset_ansi,
  print_line_bot(L).

% prints the center of the boxes (cells) and the cell content of a given line
print_line_center([]).
print_line_center([C | L]) :-
  print_cell(C),
  print_line_center(L).

print_cell(C) :-
  cell_code(C, Code),
  set_bg_color(+C), set_fg_color(+C),
  put_code(9474), write('\33\[1m'), write(Code), write('\33\[22m'), put_code(9474),
  reset_ansi.

cell_code(0, ' B ').
cell_code(1, ' W ').
cell_fg_color(0, '\33\[34m').
cell_fg_color(1, '\33\[31m').
cell_bg_color(0, '\33\[40m').
cell_bg_color(1, '\33\[47m').

set_fg_color(+C) :-
  cell_fg_color(C, Color),
  write(Color).
set_bg_color(+C) :-
  cell_bg_color(C, Color),
  write(Color).

reset_ansi :- write('\33\[0m').

% GAME LOGIC %

% switch_places(+GameState, -NewState, +X1, +Y1, +X2, +Y2) :- .

