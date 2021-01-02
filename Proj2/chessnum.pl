:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(random)).

% N SEI TODO
>>>>>>> 52563256c739d4347b991f3f6dc8285a0f3a5429

init_coord(L) :-
    length(L, 2),
    domain(L, 0, 7).

flattenList([], []).
flattenList([[X, Y] | L], [X, Y | NL]):- flattenList(L, NL).
flattenList([[X, Y, Z] | L], [X, Y, Z | NL]):- flattenList(L, NL).

elemToID([X, Y], V) :- V #= X + Y * 8.
posList([], []).
posList([Coord | L], [V | NL]):- elemToID(Coord, V), posList(L, NL).

valuesCoords([], []).
valuesCoords([Coord-_ | L1], [Coord | L2]) :-
    valuesCoords(L1, L2).

mapValues(_, []).
mapValues(Coords, [Coord-V | L1]) :-
    value(Coords, Coord, V),
    mapValues(Coords, L1).

bigger(N1, N2, N1, N2) :- N1 #> N2.
bigger(N1, N2, N2, N1) :- N1 #< N2.

% BLACKLIST [Cin-T-Cout | _]
% T: d, h, v

% Checks if we're not obstructing anyone
is_not_between([X, Y], [CinX, Y2]-h-[CoutX, Y2], V) :-
    bigger(CinX, CoutX, BigX, SmallX),
    % true if is not between
    (#\ (
        % true if is between
        Y #= Y2 
        #/\
        X #>= SmallX #/\ X #=< BigX
    )) #<=> V.
is_not_between([X, Y], [X2, CinY]-v-[X2, CoutY], V) :-
    bigger(CinY, CoutY, BigY, SmallY),
    % true if is not between
    (#\ (
        % true if is between
        X #= X2 
        #/\
        Y #>= SmallY #/\ Y #=< BigY
    )) #<=> V.
is_not_between([X, Y], [CinX, CinY]-d-[CoutX, CoutY], V) :-
    bigger(CinX, CoutX, BigX, SmallX),
    bigger(CinY, CoutY, BigY, SmallY),
    % true if is not between
    (#\ (
        % true if is between
        abs(X - CinX) #= abs(Y - CinY)
        #/\
        X #>= SmallX #/\ X #=< BigX
        #/\
        Y #>= SmallY #/\ Y #=< BigY
    )) #<=> V.

others_is_not_between(_, 1, []).
others_is_not_between(Condition, V, [OtherCoord | Others]) :-
    is_not_between(OtherCoord, Condition, 1),
    others_is_not_between(Condition, V, Others).
others_is_not_between(Condition, 0, [OtherCoord | Others]) :-
    is_not_between(OtherCoord, Condition, 0),
    others_is_not_between(Condition, _, Others).

% VALUES
% KING
valueKing([KX, KY], [X, Y], V) :-
    (abs(KX - X) #< 2 #/\ abs(KY - Y) #< 2) #<=> V.

% QUEEN
valueQueen([QX, QY], [X, Y], V, Others) :-
    QY #= Y,
    others_is_not_between([QX, QY]-h-[X, Y], V, Others).
valueQueen([QX, QY], [X, Y], V, Others) :-
    QX #= X,
    others_is_not_between([QX, QY]-v-[X, Y], V, Others).
valueQueen([QX, QY], [X, Y], V, Others) :-
    abs(QY - Y) #= abs(QX - X),
    others_is_not_between([QX, QY]-d-[X, Y], V, Others).
valueQueen([QX, QY], [X, Y], 0, _) :-
    abs(QY - Y) #\= abs(QX - X), QX #\= X, QY #\= Y.

% ROOK
% if on the same line, attacks is has line of sight (horizontal)
valueRook([RX, RY], [X, Y], V, Others) :-
    RY #= Y,
    others_is_not_between([RX, RY]-h-[X, Y], V, Others).
% if on the same column, attacks is has line of sight (vertical)
valueRook([RX, RY], [X, Y], V, Others) :-
    RX #= X,
    others_is_not_between([RX, RY]-v-[X, Y], V, Others).
% if not on the same line and column, doesn't attack
valueRook([RX, RY], [X, Y], 0, _) :-
    RX #\= X, RY #\= Y.

% BISHOP
% if on the same diagonal, attacks if has line of sight (diagonal)
valueBishop([BX, BY], [X, Y], V, Others) :-
    abs(BY - Y) #= abs(BX - X),
    others_is_not_between([BX, BY]-d-[X, Y], V, Others).
% if not on the same diagonal, doesn't attack
valueBishop([BX, BY], [X, Y], 0, _) :-
    abs(BY - Y) #\= abs(BX - X).

% KNIGHT
valueKnight([KX, KY], [X, Y], V) :-
    ((abs(KX - X) #= 1 #/\ abs(KY - Y) #= 2) #\/
        (abs(KX - X) #= 2 #/\ abs(KY - Y) #= 1)) #<=> V.

% PAWN
valuePawn([PX, PY], [X, Y], V) :-
    (PY - Y #= 1 #/\ abs(PX - X) #= 1) #<=> V.

value([King, Queen, Rook, Bishop, Knight, Pawn], Coord, V) :-
    valuePawn(Pawn, Coord, PawnV),
    valueKing(King, Coord, KingV),
    valueKnight(Knight, Coord, KnightV),
    valueRook(Rook, Coord, RookV, [King, Queen, Bishop, Knight, Pawn]),
    valueBishop(Bishop, Coord, BishopV, [King, Queen, Rook, Knight, Pawn]),
    valueQueen(Queen, Coord, QueenV, [King, Rook, Bishop, Knight, Pawn]),
    V #= KingV + QueenV + RookV + BishopV + KnightV + PawnV.

chess_num(Values, Coords) :-
    reset_timer,
    % Definir Coords pecas
    Coords = [King, Queen, Rook, Bishop, Knight, Pawn],
    init_coord(King), init_coord(Queen), init_coord(Rook),
    init_coord(Bishop), init_coord(Knight), init_coord(Pawn),
    % no overlap
    posList(Coords, PosL1),
    valuesCoords(Values, CValues), % get values coords
    posList(CValues, PosL2),
    append(PosL1, PosL2, NPosL),

    all_distinct(NPosL),
    % Valores
    mapValues(Coords, Values),
    %print_time('Posting Constraints: '),

    flattenList(Coords, L),
    labeling([ffc, bisect], L),
    %print_time('Labeling Time: '),
    fd_statistics, statistics.

% DISPLAY
%  --- --- --- --- --- --- --- ---
% | 0 |   |   |   |   |   |   |   |
%  --- --- --- --- --- --- --- ---
base_board([
    [-1, -1, -1, -1, -1, -1, -1, -1],
    [-1, -1, -1, -1, -1, -1, -1, -1],
    [-1, -1, -1, -1, -1, -1, -1, -1],
    [-1, -1, -1, -1, -1, -1, -1, -1],
    [-1, -1, -1, -1, -1, -1, -1, -1],
    [-1, -1, -1, -1, -1, -1, -1, -1],
    [-1, -1, -1, -1, -1, -1, -1, -1],
    [-1, -1, -1, -1, -1, -1, -1, -1]
]).

replace_val([_|T], 0, X, [X|T]).
replace_val([H|T], I, X, [H|R]) :-
  I > -1,
  NI is I - 1,
  replace_val(T, NI, X, R), !.
replace_val(L, _, _, L).

replace_val_matrix([H|T], 0, Col, X, [R|T]) :-
  replace_val(H, Col, X, R).
replace_val_matrix([H|T], Line, Col, X, [H|R]) :-
  Line > -1,
  Line1 is Line - 1,
  replace_val_matrix(T, Line1, Col, X, R).

gen_board(_, 8, _, _, []).
gen_board(X, Y, NumberedSquares, Coords, [Line|Lines]) :-
  Y =< 7,
  gen_board_line(X, Y, NumberedSquares, Coords, Line),
  Y1 is Y + 1,
  gen_board(X, Y1, NumberedSquares, Coords, Lines).

gen_board_line(8, _, _, _, []).
gen_board_line(X, Y, NumberedSquares, Coords, [C|Chars]) :-
  X =< 7,
  member([X, Y]-C, NumberedSquares),
  X1 is X + 1,
  gen_board_line(X1, Y, NumberedSquares, Coords, Chars).
gen_board_line(X, Y, NumberedSquares, Coords, [C|Chars]) :-
  X =< 7,
  nth0(PieceIndex, Coords, [X, Y]),
  C is -2 - PieceIndex,
  X1 is X + 1,
  gen_board_line(X1, Y, NumberedSquares, Coords, Chars).
gen_board_line(X, Y, NumberedSquares, Coords, [-1|Chars]) :-
  X =< 7,
  \+member([X, Y]-_, NumberedSquares),
  \+nth0(_, Coords, [X, Y]),
  X1 is X + 1,
  gen_board_line(X1, Y, NumberedSquares, Coords, Chars).

display_board([]) :-
    write(' --- --- --- --- --- --- --- ---'), nl.
display_board([L|Lines]) :-
    display_line_top,
    display_line(L),
    display_board(Lines).

display_line_top :-
  write(' --- --- --- --- --- --- --- ---'), nl.
display_line([]) :- write('|'), nl.
display_line([C|Chars]) :-
    display_char(C),
    display_line(Chars).

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

% THROWAWAY
ttt(C) :-
    NumberedSquares = [[1, 0]-1, [3, 0]-6],
    chess_num(NumberedSquares, C), wr(C),
    gen_board(0, 0, NumberedSquares, C, Board),
    display_board(Board).

wr(L):-
    % [[3, 1], [6, 0], [2, 0], [0, 3], [1, 1], [4, 1]|_] = L,
    format('Ki: ~w\tQ: ~w\tR: ~w\tB: ~w\tKn: ~w \tP: ~w \n', L).

test :-
    findall(C, (chess_num([[1, 0]-1, [3, 0]-6, [4, 2]-2, [3, 4]-0], C), wr(C)), _).

    % findall(C, (chess_num([[2, 1]-4, [0, 5]-0, [6, 3]-4, [2, 7]-4], C), wr(C)), _).
    % findall(C, (chess_num([[0, 0]-1, [1, 0]-0, [5, 0]-0, [7, 0]-1, [0, 2]-0, [3, 3]-0, [4, 3]-0, [7, 3]-0, [6, 4]-0, [7, 4]-0, [5, 6]-0, [5, 7]-0, [7, 7]-1], C), wr(C)), _).
    % findall(C, (chess_num([[2, 0]-0, [3, 0]-0, [2, 1]-1, [4, 1]-1, [2, 2]-2, [4, 2]-2,
                           %[2, 3]-3, [4, 3]-3, [2, 4]-4, [4, 4]-4, [6, 7]-0], C), wr(C)), _).
    % findall(C, (chess_num([[0, 0]-0, [7, 0]-0, [0, 6]-0, [0, 7]-0, [6, 7]-0,
    % [2, 2]-1, [3, 2]-1, [4, 2]-1, [5, 2]-1, [2, 3]-1, [3, 3]-1, [4, 3]-1, [5, 3]-1,
    % [2, 4]-1, [3, 4]-1, [4, 4]-1, [5, 4]-1], [2, 5]-1, [3, 5]-1, [4, 5]-1, [5, 5]-1], C), wr(C)), _).

reset_timer:-
    statistics(total_runtime, _).

print_time(Msg) :-
    statistics(total_runtime, [_, T]),
    TS is ((T // 10) * 10) / 1000, nl,
    write(Msg), write(TS), write('s'), nl, nl.

% GEN PROBLEM

selRandom(Var, BB0, BB1) :-
    fd_set(Var, Set), fdset_to_list(Set, List),
    random_member(Value, List),
    (first_bound(BB0, BB1), Var #= Value ;
     later_bound(BB0, BB1), Var #\= Value).

initValues([]).
initValues([Coord-Score | Values]) :-
    length(Coord, 2),
    domain(Coord, 0, 6),
    %domain([Score], 0, 7),
    initValues(Values).

valuesToCoordList([], []).
valuesToCoordList([[X, Y]-_ | Values], [[X, Y] | L]) :-
    valuesToCoordList(Values, L).
valuesToScoreList([], []).
valuesToScoreList([_-Score | Values], [Score | L]) :-
    valuesToScoreList(Values, L).

gen_problem(NCells, Coords, Values) :-
    Coords = [King, Queen, Rook, Bishop, Knight, Pawn],
    init_coord(King), init_coord(Queen), init_coord(Rook),
    init_coord(Bishop), init_coord(Knight), init_coord(Pawn),

    length(Values, NCells),
    initValues(Values),

    % All Distinct
    valuesCoords(Values, CValues), % get values coords

    posList(Coords, PosL1),
    posList(CValues, PosL2),
    append(PosL1, PosL2, NPosL),
    all_distinct(NPosL),

    mapValues(Coords, Values),

    valuesToCoordList(Values, CoordValues),

    append(Coords, CoordValues, L),
    flattenList(L, NNL),
    %valuesToScoreList(Values, ScoreValues),
    %append(NL, ScoreValues, NNL),
    labeling([value(selRandom)], NNL).
