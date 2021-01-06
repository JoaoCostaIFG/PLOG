:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(random)).

:-include('chessnum_display.pl').
:-include('chessnum_gen_problem.pl').
:-include('chessnum_tests.pl').

init_coord(L) :-
    length(L, 2),
    domain(L, 0, 7).

flattenList([], []).
flattenList([[X, Y] | L], [X, Y | NL]):- flattenList(L, NL).
flattenList([[X, Y, Z] | L], [X, Y, Z | NL]):- flattenList(L, NL).

elemToID([X, Y], V) :- V #= X + Y * 8.
coordToIndexList([], []).
coordToIndexList([Coord | L], [V | NL]):-
  elemToID(Coord, V), coordToIndexList(L, NL).

stripNumberedSquareValue([], []).
stripNumberedSquareValue([Coord-_ | L1], [Coord | L2]) :-
    stripNumberedSquareValue(L1, L2).

mapValues(_, []).
mapValues(Coords, [Coord-V | L1]) :-
    value(Coords, Coord, V),
    mapValues(Coords, L1).

% BLACKLIST [Cin-T-Cout | _]
% T: d, h, v

% Checks if we're not obstructing anyone
is_not_between([X, Y], [CinX, Y2]-h-[CoutX, Y2], V) :-
    % true if is not between
    (#\ (
        % true if is between
        Y #= Y2 
        #/\
        abs(CinX - CoutX) #= abs(CoutX - X) + abs(CinX - X) 
    )) #<=> V.
is_not_between([X, Y], [X2, CinY]-v-[X2, CoutY], V) :-
    % true if is not between
    (#\ (
        % true if is between
        X #= X2 
        #/\
        abs(CinY - CoutY) #= abs(CoutY - Y) + abs(CinY - Y) 
    )) #<=> V.
is_not_between([X, Y], [CinX, CinY]-d-[CoutX, CoutY], V) :-
    % true if is not between
    (#\ (
        % true if is between
        abs(X - CoutX) #= abs(Y - CoutY)
        #/\
        abs(CinY - CoutY) #= abs(CoutY - Y) + abs(CinY - Y) 
        #/\
        abs(CinX - CoutX) #= abs(CoutX - X) + abs(CinX - X) 
    )) #<=> V.

others_is_not_between(_, 1, []).
others_is_not_between(Condition, V, [OtherCoord | Others]) :-
    is_not_between(OtherCoord, Condition, 1),
    others_is_not_between(Condition, V, Others).
others_is_not_between(Condition, 0, [OtherCoord | _Others]) :-
    is_not_between(OtherCoord, Condition, 0).
     %others_is_not_between(Condition, _, Others).

%others_is_not_between(Condition, Output, [P1, P2, P3, P4, P5]) :-
    %is_not_between(P1, Condition, V1),
    %is_not_between(P2, Condition, V2),
    %is_not_between(P3, Condition, V3),
    %is_not_between(P4, Condition, V4),
    %is_not_between(P5, Condition, V5),
    %(
        %(V1 #/\ V2 #/\ V3 #/\ V4 #/\ V5) #<=> Output
    %).

% VALUES
% These predicates return 1 in V if the given piece can attack the numbered square
% and 0 otherwise
% KING
valueKing([KX, KY], [X, Y], V) :-
    (abs(KX - X) #< 2 #/\ abs(KY - Y) #< 2) #<=> V.

% QUEEN
% if on the same line, attacks is has line of sight (horizontal)
valueQueen([QX, QY], [X, Y], V, Others) :-
    QY #= Y,
    others_is_not_between([QX, QY]-h-[X, Y], V, Others).
% if on the same column, attacks is has line of sight (vertical)
valueQueen([QX, QY], [X, Y], V, Others) :-
    QX #= X,
    others_is_not_between([QX, QY]-v-[X, Y], V, Others).
% if on the same diagonal, attacks if has line of sight (diagonal)
valueQueen([QX, QY], [X, Y], V, Others) :-
    abs(QY - Y) #= abs(QX - X),
    others_is_not_between([QX, QY]-d-[X, Y], V, Others).
% if not on the same line, column, and diagonal doesn't attack
valueQueen([QX, QY], [X, Y], 0, _) :-
    abs(QY - Y) #\= abs(QX - X), QX #\= X, QY #\= Y.

%valueQueen([QX, QY], [X, Y], V, Others) :-
    %bigger(QY, Y, BigY, SmallY),
    %bigger(QX, X, BigX, SmallX),
    %others_is_not_between([SmallX, SmallY]-d-[BigX, BigY]-[X, Y], VDiag, Others),
    %others_is_not_between([SmallX, QY]-h-[BigX, Y], VHoriz, Others),
    %others_is_not_between([QX, SmallY]-v-[X, BigY], VVert, Others),
    %(
        %(QY #= Y #/\ VHoriz)
        %#\/
        %(QX #= X #/\ VVert)
        %#\/
        %((abs(QY - Y) #= abs(QX - X)) #/\ VDiag)
    %) #<=> V.

% ROOK
% if on the same line, attacks is has line of sight
% (horizontal)
valueRook([RX, RY], [X, Y], V, Others) :-
    RY #= Y,
    others_is_not_between([RX, RY]-h-[X, Y], V, Others).
% if on the same column, attacks is has line of sight
% (vertical)
valueRook([RX, RY], [X, Y], V, Others) :-
    RX #= X,
    others_is_not_between([RX, RY]-v-[X, Y], V, Others).
% if not on the same line and column, doesn't attack
valueRook([RX, RY], [X, Y], 0, _) :-
    RX #\= X, RY #\= Y.

% BISHOP
% if on the same diagonal, attacks if has line of sight (diagonal)
%valueBishop([BX, BY], [X, Y], V, Others) :-
    %abs(BY - Y) #= abs(BX - X),
    %others_is_not_between([BX, BY]-d-[X, Y], V, Others).
%% if not on the same diagonal, doesn't attack
%valueBishop([BX, BY], [X, Y], 0, _) :-
    %abs(BY - Y) #\= abs(BX - X).
valueBishop([BX, BY], [X, Y], V, Others) :-
    others_is_not_between([BX, BY]-d-[X, Y], VDiag, Others),
    (
        (abs(BY - Y) #= abs(BX - X) #/\ VDiag) #<=> V
    ).
    

% KNIGHT
valueKnight([KX, KY], [X, Y], V) :-
    ((abs(KX - X) #= 1 #/\ abs(KY - Y) #= 2) #\/
        (abs(KX - X) #= 2 #/\ abs(KY - Y) #= 1)) #<=> V.

% PAWN
valuePawn([PX, PY], [X, Y], V) :-
    (PY - Y #= 1 #/\ abs(PX - X) #= 1) #<=> V.

% Constraints the pieces coordinates to not attack a
% numbered square that can't be attacked.
value([King, Queen, Rook, Bishop, Knight, Pawn], Coord, 0) :-
    valueKing(King, Coord, 0),
    valueKnight(Knight, Coord, 0),
    valuePawn(Pawn, Coord, 0),
    valueQueen(Queen, Coord, 0, [King, Rook, Bishop, Knight, Pawn]),
    valueRook(Rook, Coord, 0, [King, Queen, Bishop, Knight, Pawn]),
    valueBishop(Bishop, Coord, 0, [King, Queen, Rook, Knight, Pawn]).
% Constraints the pieces coordinates to attack a
% numbered square the given number of times.
value([King, Queen, Rook, Bishop, Knight, Pawn], Coord, V) :-
    V #\= 0,
    valuePawn(Pawn, Coord, PawnV),
    valueKing(King, Coord, KingV),
    valueKnight(Knight, Coord, KnightV),
    valueRook(Rook, Coord, RookV, [King, Queen, Bishop, Knight, Pawn]),
    valueBishop(Bishop, Coord, BishopV, [King, Queen, Rook, Knight, Pawn]),
    valueQueen(Queen, Coord, QueenV, [King, Rook, Bishop, Knight, Pawn]),
    V #= KingV + QueenV + RookV + BishopV + KnightV + PawnV.

chess_num(NumberedSquares, Coords) :-
    Coords = [King, Queen, Rook, Bishop, Knight, Pawn],
    reset_timer,
    % init the coordinates of each piece
    init_coord(King), init_coord(Queen), init_coord(Rook),
    init_coord(Bishop), init_coord(Knight), init_coord(Pawn),

    % for alldistinct
    % get values coords
    stripNumberedSquareValue(NumberedSquares, NumberedSquaresCoords),
    coordToIndexList(NumberedSquaresCoords, PosL1),
    coordToIndexList(Coords, PosL2),
    append(PosL1, PosL2, PosL),
    % all cordinates of pieces and numbered squares have to be distinct
    all_distinct(PosL),

    % solve the problem
    mapValues(Coords, NumberedSquares),

    % UNCOMENT THESE TO GET STATITICS (be careful to change the final
    % dot position)
    %print_time('Posting Constraints: '),
    flattenList(Coords, L),
    labeling([ffc, bisect], L).
    % print_time('Labeling Time: '),
    % fd_statistics, statistics.

% TIMER
reset_timer:-
    statistics(total_runtime, _).
print_time(Msg) :-
    statistics(total_runtime, [_, T]),
    TS is ((T // 10) * 10) / 1000, nl,
    write(Msg), write(TS), write('s'), nl, nl.
