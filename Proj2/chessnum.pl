:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(random)).

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
    % true if is not between
    (#\ (
        % true if is between
        Y #= Y2 
        #/\
        CinX #=< X #/\ X #=< CoutX
    )) #<=> V.
is_not_between([X, Y], [X2, CinY]-v-[X2, CoutY], V) :-
    % true if is not between
    (#\ (
        % true if is between
        X #= X2 
        #/\
        CinY #=< Y #/\ Y #=< CoutY
    )) #<=> V.
is_not_between([X, Y], [CinX, CinY]-d-[CoutX, CoutY]-[TX, TY], V) :-
    % true if is not between
    (#\ (
        % true if is between
        abs(X - TX) #= abs(Y - TY)
        #/\
        CinX #=< X #/\ X #=< CoutX
        #/\
        CinY #=< Y #/\ Y #=< CoutY
    )) #<=> V.

others_is_not_between(_, 1, []).
others_is_not_between(Condition, V, [OtherCoord | Others]) :-
    is_not_between(OtherCoord, Condition, 1),
    others_is_not_between(Condition, V, Others).
others_is_not_between(Condition, 0, [OtherCoord | _Others]) :-
    is_not_between(OtherCoord, Condition, 0).
    % others_is_not_between(Condition, _, Others).

% VALUES
% KING
valueKing([KX, KY], [X, Y], V) :-
    (abs(KX - X) #< 2 #/\ abs(KY - Y) #< 2) #<=> V.

% QUEEN
valueQueen([QX, QY], [X, Y], V, Others) :-
    QY #= Y,
    bigger(QX, X, BigX, SmallX),
    others_is_not_between([SmallX, QY]-h-[BigX, Y], V, Others).
valueQueen([QX, QY], [X, Y], V, Others) :-
    QX #= X,
    bigger(QY, Y, BigY, SmallY),
    others_is_not_between([QX, SmallY]-v-[X, BigY], V, Others).
valueQueen([QX, QY], [X, Y], V, Others) :-
    abs(QY - Y) #= abs(QX - X),
    bigger(QX, X, BigX, SmallX),
    bigger(QY, Y, BigY, SmallY),
    others_is_not_between([SmallX, SmallY]-d-[BigX, BigY]-[X, Y], V, Others).
valueQueen([QX, QY], [X, Y], 0, _) :-
    abs(QY - Y) #\= abs(QX - X), QX #\= X, QY #\= Y.

% ROOK
% if on the same line, attacks is has line of sight (horizontal)
valueRook([RX, RY], [X, Y], V, Others) :-
    RY #= Y,
    bigger(RX, X, BigX, SmallX),
    others_is_not_between([SmallX, RY]-h-[BigX, Y], V, Others).
% if on the same column, attacks is has line of sight (vertical)
valueRook([RX, RY], [X, Y], V, Others) :-
    RX #= X,
    bigger(RY, Y, BigY, SmallY),
    others_is_not_between([RX, SmallY]-v-[X, BigY], V, Others).
% if not on the same line and column, doesn't attack
valueRook([RX, RY], [X, Y], 0, _) :-
    RX #\= X, RY #\= Y.

% BISHOP
% if on the same diagonal, attacks if has line of sight (diagonal)
valueBishop([BX, BY], [X, Y], V, Others) :-
    abs(BY - Y) #= abs(BX - X),
    bigger(BX, X, BigX, SmallX),
    bigger(BY, Y, BigY, SmallY),
    others_is_not_between([SmallX, SmallY]-d-[BigX, BigY]-[X, Y], V, Others).
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

value([King, Queen, Rook, Bishop, Knight, Pawn], Coord, 0) :-
    valueKing(King, Coord, 0),
    valueKnight(Knight, Coord, 0),
    valuePawn(Pawn, Coord, 0),
    valueQueen(Queen, Coord, 0, [King, Rook, Bishop, Knight, Pawn]),
    valueRook(Rook, Coord, 0, [King, Queen, Bishop, Knight, Pawn]),
    valueBishop(Bishop, Coord, 0, [King, Queen, Rook, Knight, Pawn]).
value([King, Queen, Rook, Bishop, Knight, Pawn], Coord, V) :-
    V #\= 0,
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
    print_time('Labeling Time: '),
    fd_statistics, statistics.

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

% THROWAWAY
t(NumberedSquares, C):-
    write('Solving for'), nl,
    display_board(NumberedSquares),
    chess_num(NumberedSquares, C),
    format('Ki: ~w\tQ: ~w\tR: ~w\tB: ~w\tKn: ~w \tP: ~w \n', C),
    display_board(NumberedSquares, C).

test1(C) :-
    % sol: [[3, 1], [6, 0], [2, 0], [0, 3], [1, 1], [4, 1]]
    t([[1, 0]-1, [3, 0]-6, [4, 2]-2, [3, 4]-0], C).

test2(C) :-
    t([[2, 1]-4, [0, 5]-0, [6, 3]-4, [2, 7]-4], C).

test3(C) :-
    t([[0, 0]-1, [1, 0]-0, [5, 0]-0, [7, 0]-1, [0, 2]-0, [3, 3]-0, [4, 3]-0, [7, 3]-0, [6, 4]-0, [7, 4]-0, [5, 6]-0, [5, 7]-0, [7, 7]-1], C).

test4(C) :-
    t([[2, 1]-3, [7, 1]-0, [4, 4]-5, [3, 6]-3, [7, 6]-0], C).

test5(C) :-
    t([[1, 0]-0, [2, 0]-0, [5, 0]-0, [7, 1]-0, [7, 2]-0, [0, 4]-0, [5, 4]-4, [7, 4]-0, [7, 6]-0, [2, 7]-0, [5, 7]-0, [6, 7]-0, [7, 7]-0], C).

test6(C) :-
    t([[0, 0]-0, [7, 0]-0, [0, 6]-0, [0, 7]-0, [6, 7]-0,
  [2, 2]-1, [3, 2]-1, [4, 2]-1, [5, 2]-1,
  [2, 3]-1, [3, 3]-1, [4, 3]-1, [5, 3]-1,
  [2, 4]-1, [3, 4]-1, [4, 4]-1, [5, 4]-1,
  [2, 5]-1, [3, 5]-1, [4, 5]-1, [5, 5]-1], C).

test7(C) :-
    t([[0, 1]-1, [1, 1]-1, [2, 1]-1, [3, 1]-1, [4, 1]-1, [5, 1]-1, [6, 1]-1, [7, 1]-1,
  [0, 6]-1, [1, 6]-1, [2, 6]-1, [3, 6]-2, [4, 6]-2, [5, 6]-1, [6, 6]-1, [7, 6]-2,
  [5, 7]-2, [6, 7]-1], C).

test8(C) :-
    t([[0, 0]-0, [0, 1]-0, [1, 1]-0, [1, 2]-0, [2, 2]-0, [2, 3]-0, [3, 3]-0, [3, 4]-0, [4, 4]-0, [4, 5]-0, [5, 5]-0, [5, 6]-0, [6, 6]-0, [6, 7]-0, [7, 7]-0, [2, 6]-2], C).

test9(C) :-
    t([[2, 0]-0, [3, 0]-0, [2, 1]-1, [4, 1]-1, [2, 2]-2, [4, 2]-2, [2, 3]-3, [4, 3]-3, [2, 4]-4, [4, 4]-4, [6, 7]-0], C).

test10(C) :-
    t([[0, 0]-0, [1, 0]-0, [2, 0]-0, [3, 0]-0, [4, 0]-0, [5, 0]-0, [6, 0]-0, [7, 0]-0,
  [0, 1]-1, [1, 1]-1, [2, 1]-1, [3, 1]-1, [4, 1]-1, [5, 1]-1, [6, 1]-1, [7, 1]-1,
  [0, 5]-1, [2, 6]-2, [4, 7]-3], C).

test11(C) :-
    t([[2, 1]-2, [6, 1]-3, [3, 2]-2, [1, 4]-3, [6, 4]-3, [4, 6]-3, [2, 7]-1], C).

reset_timer:-
    statistics(total_runtime, _).

print_time(Msg) :-
    statistics(total_runtime, [_, T]),
    TS is ((T // 10) * 10) / 1000, nl,
    write(Msg), write(TS), write('s'), nl, nl.

% GEN PROBLEM
selRandom(Var, _Rest, BB0, BB1) :-
    fd_set(Var, Set), fdset_to_list(Set, List),
    random_member(Value, List),
    (first_bound(BB0, BB1), Var #= Value ;
     later_bound(BB0, BB1), Var #\= Value).

initValues([]).
initValues([Coord-Score | Values]) :-
    length(Coord, 2),
    domain(Coord, 0, 7),
    % redundant but improves efficiency
    % forbidding 0 on value domain to get more interesting puzzles
    domain([Score], 1, 6),
    initValues(Values).

valuesToCoordList([], []).
valuesToCoordList([[X, Y]-_ | Values], [[X, Y] | L]) :-
    valuesToCoordList(Values, L).

numList(I, N, []) :- I >= N.
numList(I, N, [I | T]) :-
    I < N,
    I1 is I + 1,
    numList(I1, N, T).
permutatedIndsToCoords(_, []).
permutatedIndsToCoords([Ind | PermutatedCoords], [[PieceX, PieceY] | Pieces]) :-
    PieceX is mod(Ind, 8),
    PieceY is div(Ind, 8),
    permutatedIndsToCoords(PermutatedCoords, Pieces).
gen_pieceCoords([King, Queen, Rook, Bishop, Knight, Pawn]) :-
    % generate 6 random (distinct) coordinates for the game pieces
    numList(0, 64, PossibleCoords),
    random_permutation(PossibleCoords, PermutatedCoords),
    permutatedIndsToCoords(PermutatedCoords, [King, Queen, Rook, Bishop, Knight, Pawn]).
    
gen_problem(NCells, [King, Queen, Rook, Bishop, Knight, Pawn], Values) :-
    NCells < 57, % if we ask for more than
    Coords = [King, Queen, Rook, Bishop, Knight, Pawn],
    gen_pieceCoords(Coords),

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

    labeling([value(selRandom)], NNL),
    display_board(Values),
    display_board(Values, Coords).
