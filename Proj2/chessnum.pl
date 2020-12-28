:-use_module(library(clpfd)).
:-use_module(library(between)).

% N SEI TODO

init_coord(L) :-
    length(L, 2),
    domain(L, 0, 7).

flattenList([], []).
flattenList([[X, Y] | L], [X, Y | NL]):- flattenList(L, NL).

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
    #\ (
        % true if is between
        Y #= Y2 
        #/\
        X #>= SmallX #/\ X #=< BigX
    ) #=> V.
is_not_between([X, Y], [[X2, CinY]-v-[X2, CoutY] | L], V) :-
    bigger(CinY, CoutY, BigY, SmallY),
    % true if is not between
    #\ (
        % true if is between
        X #= X2 
        #/\
        Y #>= SmallY #/\ Y #=< BigY
    ) #=> V.
is_not_between([X, Y], [[CinX, CinY]-d-[CoutX, CoutY] | L], V) :-
    bigger(CinX, CoutX, BigX, SmallX),
    bigger(CinY, CoutY, BigY, SmallY),
    % true if is not between
    #\ (
        % true if is between
        abs(X - CinX) #= abs(Y - CinY)
        #/\
        X #>= SmallX #/\ X #=< BigX
        #/\
        Y #>= SmallY #/\ Y #=< BigY
    ) #=> V.

others_is_not_between(_, 1, []).
others_is_not_between(Conditions, V, [OtherCoord | Others]) :-
    is_not_between(OtherCoord, Conditions, 1),
    others_is_not_between(Conditions, V, Others).
others_is_not_between(Conditions, 0, [OtherCoord | Others]) :-
    is_not_between(OtherCoord, Conditions, 0),
    others_is_not_between(Conditions, _, Others).

% VALUES
% KING
valueKing([KX, KY], [X, Y], V) :-
    (abs(KX - X) #< 2 #/\ abs(KY - Y) #< 2) #<=> V.

% QUEEN
valueQueen([QX, QY], [X, Y], V, Others) :-
    QY #= Y,
    others_is_not_between([[QX, QY]-h-[X, Y]], V, Others).
valueQueen([QX, QY], [X, Y], V, Others) :-
    QX #= X,
    others_is_not_between([[QX, QY]-v-[X, Y]], V, Others).
valueQueen([QX, QY], [X, Y], V, Others) :-
    abs(QY - Y) #= abs(QX - X),
    others_is_not_between([[QX, QY]-d-[X, Y]], V, Others).
valueQueen([QX, QY], [X, Y], 0, _) :-
    abs(QY - Y) #\= abs(QX - X), QX #\= X, QY #\= Y.

% ROOK
valueRook([RX, RY], [X, Y], V, Others) :-
    RY #= Y,
    others_is_not_between([[RX, RY]-h-[X, Y]], V, Others).
valueRook([RX, RY], [X, Y], V, Others) :-
    RX #= X,
    others_is_not_between([[RX, RY]-v-[X, Y]], V, Others).
valueRook([RX, RY], [X, Y], 0, _) :-
    RX #\= X, RY #\= Y.

% BISHOP
valueBishop([BX, BY], [X, Y], V, Others) :-
    abs(BY - Y) #= abs(BX - X),
    others_is_not_between([[BX, BY]-d-[X, Y]], V, Others).
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
    %is_not_between(Pawn, BlackListIn), 
    %is_not_between(King, BlackListIn), 
    %is_not_between(Knight, BlackListIn), 
    %is_not_between(Rook, BlackListIn),
    %is_not_between(Bishop, BlackListOutRook),
    %is_not_between(Queen, BlackListOutBishop).

chess_num(Values, Coords) :-
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

    flattenList(Coords, L),
    labeling([], L).


% THROWAWAY
wr(L):-
    % [[3, 1], [6, 0], [2, 0], [0, 3], [1, 1], [4, 1]|_] = L,

    format('Ki: ~w\tQ: ~w\tR: ~w\tB: ~w\tKn: ~w \tP: ~w \n', L).
wr(_).

test :-
     %findall(C, (chess_num([[1, 0]-1, [3, 0]-6, [4, 2]-2, [3, 4]-0], C), wr(C)), _).

    % findall(C, (chess_num([[2, 1]-4, [0, 5]-0, [6, 3]-4, [2, 7]-4], C), wr(C)), _).
     findall(C, (chess_num([[0, 0]-1, [1, 0]-0, [5, 0]-0, [7, 0]-1, [0, 2]-0, [3, 3]-0, [4, 3]-0, [7, 3]-0, [6, 4]-0, [7, 4]-0, [5, 6]-0, [5, 7]-0, [7, 7]-1], C), wr(C)), _).
     %findall(C, (chess_num([[2, 0]-0, [3, 0]-0, [2, 1]-1, [4, 1]-1, [2, 2]-2, [4, 2]-2,
                           %[2, 3]-3, [4, 3]-3, [2, 4]-4, [4, 4]-4, [6, 7]-0], C), wr(C)), _).
    % findall(C, (chess_num([[0, 0]-0, [7, 0]-0, [0, 6]-0, [0, 7]-0, [6, 7]-0,
    % [2, 2]-1, [3, 2]-1, [4, 2]-1, [5, 2]-1, [2, 3]-1, [3, 3]-1, [4, 3]-1, [5, 3]-1,
    % [2, 4]-1, [3, 4]-1, [4, 4]-1, [5, 4]-1], [2, 5]-1, [3, 5]-1, [4, 5]-1, [5, 5]-1], C), wr(C)), _).

ttt(C) :-
    chess_num([[1, 0]-1, [3, 0]-6, [4, 2]-2, [3, 4]-0], C), wr(C).
