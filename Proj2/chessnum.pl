:-use_module(library(clpfd)).
:-use_module(library(between)).

% N SEI TODO

init_coord(L) :-
    length(L, 2),
    domain(L, 0, 7).

formList([], []).
formList([[X, Y] | L], [X, Y | NL]):- formList(L, NL).

elemToID([X, Y], V) :- V #= X + Y * 8.
posList([], []).
posList([Coord | L], [V | NL]):- elemToID(Coord, V), posList(L, NL).

valuesCoords([], []).
valuesCoords([Coord-_ | L1], [Coord | L2]) :-
    valuesCoords(L1, L2).

mapValues(Coords, L) :- mapValues(Coords, L, []).
mapValues(_, [], _).
mapValues(Coords, [Coord-V | L1], BlackList) :-
    value(Coords, Coord, V, BlackList, BlackListOut),
    mapValues(Coords, L1, BlackListOut).

% BLACKLIST [C_IN-T-C_OUT, ]
% T: d, h, v
is_not_between(_, []).
is_not_between([X, Y], [[CinX, Y]-h-[CoutX, Y] | L]) :-
    #\ (
        (X #> CinX #/\ X #< CoutX)
        #\/
        (X #< CinX #/\ X #> CoutX)
    ),
    is_not_between([X, Y], L).
is_not_between([X, Y], [[X, CinY]-v-[X, CoutY] | L]) :-
    #\ (
        (Y #> CinY #/\ Y #< CoutY)
        #\/
        (Y #< CinY #/\ Y #> CoutY)
    ),
    is_not_between([X, Y], L).
is_not_between([X, Y], [_ | L]) :-
    is_not_between([X, Y], L).

% VALUES
valueKing([KX, KY], [X, Y], V) :-
    DiffX #= KX - X,
    DiffY #= KY - Y,
    (abs(DiffX) #< 2 #/\ abs(DiffY) #<2) #<=> V.

valueQueen([QX, QY], [X, Y], V, BlackList) :-
    ((abs(QY - Y) #= abs(QX - X)) #\/ (QX #= X) #\/ (QY #= Y)) #<=> V.

valueRook([RX, RY], [X, Y], 1, [RY-v-Y | BlackList]) :- 
    RX #= X.
valueRook([RX, RY], [X, Y], 1, [RX-h-X | BlackList]) :- 
    RY #= Y.
valueRook([RX, RY], [X, Y], 0, BlackList) :-
    RX #\= X, RY #\= Y.

valueBishop([BX, BY], [X, Y], V, BlackList) :-
    (abs(BY - Y) #= abs(BX - X)) #<=> V.

valueKnight([KX, KY], [X, Y], V) :-
    ((abs(KX - X) #= 1 #/\ abs(KY - Y) #= 2) #\/
        (abs(KX - X) #= 2 #/\ abs(KY - Y) #= 1)) #<=> V.

valuePawn([PX, PY], [X, Y], V) :-
    (PY - Y #= 1 #/\ abs(PX - X) #= 1) #<=> V.

value([King, Queen, Rook, Bishop, Knight, Pawn], Coord, V, BlackListIn, BlackListOut) :-
    valuePawn(Pawn, Coord, PawnV), is_not_between(Pawn, BlackListIn),
    valueKing(King, Coord, KingV), %is_not_between(King, BlackListIn),
    valueKnight(Knight, Coord, KnightV), %is_not_between(Knight, BlackListIn),
    valueRook(Rook, Coord, RookV, BlackListOutRook), %is_not_between(Rook, BlackListIn),
    valueBishop(Bishop, Coord, BishopV, BlackListOutBishop), %is_not_between(Bishop, BlackListOutRook),
    valueQueen(Queen, Coord, QueenV, BlackListOut), %is_not_between(Queen, BlackListOutBishop),
    V #= KingV + QueenV + RookV + BishopV + KnightV + PawnV.

chess_num(Values, Coords) :-
    % Definir Coords pecas
    % TODO Subst por maplist
    init_coord(King), init_coord(Queen), init_coord(Rook),
    init_coord(Bishop), init_coord(Knight), init_coord(Pawn),
    Coords = [King, Queen, Rook, Bishop, Knight, Pawn],
    posList(Coords, PosL1),

    valuesCoords(Values, CValues),
    posList(CValues, PosL2),
    append(PosL1, PosL2, NPosL),
    all_distinct(NPosL),

    % Valores
    mapValues(Coords, Values),

    formList(Coords, L),
    labeling([], L).

wr(L):-
    format('K: ~w\tQ: ~w\tR: ~w\tB: ~w\tK: ~w \tP: ~w \n', L).

test :-
  findall(C, (chess_num([[1, 0]-1, [3, 0]-6, [4, 2]-2, [3, 4]-0], C), wr(C)), L).
