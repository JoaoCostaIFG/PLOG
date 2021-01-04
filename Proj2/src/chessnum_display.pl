% DISPLAY
%  --- --- --- --- --- --- --- ---
% | 0 |   |   |   |   |   |   |   |
%  --- --- --- --- --- --- --- ---
display_board(NumberedSquares) :-
  display_board(0, 0, NumberedSquares, []).
display_board(NumberedSquares, Coords) :-
  display_board(0, 0, NumberedSquares, Coords).
display_board(_, 8, _, _) :-
    write(' --- --- --- --- --- --- --- ---'), nl.
display_board(X, Y, NumberedSquares, Coords) :-
    Y =< 7,
    display_line_top,
    display_line(X, Y, NumberedSquares, Coords),
    Y1 is Y + 1,
    display_board(X, Y1, NumberedSquares, Coords).

display_line_top :-
  write(' --- --- --- --- --- --- --- ---'), nl.
display_line(8, _, _, _) :- write('|'), nl.
display_line(X, Y, NumberedSquares, Coords) :-
    X =< 7,
    member([X, Y]-C, NumberedSquares),
    display_char(C),
    X1 is X + 1,
    display_line(X1, Y, NumberedSquares, Coords).
display_line(X, Y, NumberedSquares, Coords) :-
    X =< 7,
    nth0(PieceIndex, Coords, [X, Y]),
    C is -2 - PieceIndex,
    display_char(C),
    X1 is X + 1,
    display_line(X1, Y, NumberedSquares, Coords).
display_line(X, Y, NumberedSquares, Coords) :-
    X =< 7,
    \+member([X, Y]-_, NumberedSquares),
    \+nth0(_, Coords, [X, Y]),
    display_char(-1),
    X1 is X + 1,
    display_line(X1, Y, NumberedSquares, Coords).

display_char(-1) :- write('|   '), !. % green cuts
display_char(-2) :- write('| K '), !.
display_char(-3) :- write('| Q '), !.
display_char(-4) :- write('| R '), !.
display_char(-5) :- write('| B '), !.
display_char(-6) :- write('| Kn'), !.
display_char(-7) :- write('| P '), !.
display_char(C) :-
    C >= 0,
    format('| ~w ', [C]).

% show the list of piece coordinates formatted on screen
format_piece_coord(Coords) :-
    format('K: ~w\tQ: ~w\tR: ~w\tB: ~w\tKn: ~w \tP: ~w \n', Coords).

% solves and problem and shows the solution formated
chess_num_graphic(NumberedSquares, C):-
    write('Solving for'), nl,
    display_board(NumberedSquares),
    chess_num(NumberedSquares, C),
    format_piece_coord(C),
    display_board(NumberedSquares, C).
