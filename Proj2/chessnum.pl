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

mapValues(Coords, L) :- mapValues(Coords, L, []).
mapValues(_, [], _).
mapValues(Coords, [Coord-V | L1], BlackList) :-
    value(Coords, Coord, V, BlackList, BlackListOut),
    mapValues(Coords, L1, BlackListOut).

% BLACKLIST [Cin-T-Cout | _]
% T: d, h, v
is_not_between(_, []).
is_not_between([X, Y], [[CinX, Y2]-h-[CoutX, Y2] | L]) :-
    % true if is not between
    #\ (
        % true if is between
        Y #= Y2 
        #/\
        ((X #> CinX #/\ X #< CoutX)
        #\/
        (X #< CinX #/\ X #> CoutX))
    ),
    is_not_between([X, Y], L).
is_not_between([X, Y], [[X2, CinY]-v-[X2, CoutY] | L]) :-
    % true if is not between
    #\ (
        % true if is between
        X #= X2 
        #/\
        ((Y #> CinY #/\ Y #< CoutY)
        #\/
        (Y #< CinY #/\ Y #> CoutY))
    ),
    is_not_between([X, Y], L).

% VALUES
valueKing([KX, KY], [X, Y], V) :-
    (abs(KX - X) #< 2 #/\ abs(KY - Y) #< 2) #<=> V.

valueQueen([QX, QY], [X, Y], V, BlackList, BlackList) :-
    ((abs(QY - Y) #= abs(QX - X)) #\/ (QX #= X) #\/ (QY #= Y)) #<=> V.

valueRook([RX, RY], [X, Y], 1, BlackList, [[RX, RY]-v-[X, Y] | BlackList]) :-
    RX #= X.
valueRook([RX, RY], [X, Y], 1, BlackList, [[RX, RY]-h-[X, Y] | BlackList]) :-
    RY #= Y.
valueRook([RX, RY], [X, Y], 0, BlackList, BlackList) :-
    RX #\= X, RY #\= Y.

valueBishop([BX, BY], [X, Y], V, BlackList, BlackList) :-
    (abs(BY - Y) #= abs(BX - X)) #<=> V.

valueKnight([KX, KY], [X, Y], V) :-
    ((abs(KX - X) #= 1 #/\ abs(KY - Y) #= 2) #\/
        (abs(KX - X) #= 2 #/\ abs(KY - Y) #= 1)) #<=> V.

valuePawn([PX, PY], [X, Y], V) :-
    (PY - Y #= 1 #/\ abs(PX - X) #= 1) #<=> V.

value([King, Queen, Rook, Bishop, Knight, Pawn], Coord, V, BlackListIn, BlackListOut) :-
    valuePawn(Pawn, Coord, PawnV), %is_not_between(Pawn, BlackListIn),
    valueKing(King, Coord, KingV), %is_not_between(King, BlackListIn),
    valueKnight(Knight, Coord, KnightV), %is_not_between(Knight, BlackListIn),
    valueRook(Rook, Coord, RookV, BlackListIn, BlackListOutRook), %is_not_between(Rook, BlackListIn),
    valueBishop(Bishop, Coord, BishopV, BlackListOutRook, BlackListOutBishop), %is_not_between(Bishop, BlackListOutRook),
    valueQueen(Queen, Coord, QueenV, BlackListOutBishop, BlackListOut), %is_not_between(Queen, BlackListOutBishop),
    V #= KingV + QueenV + RookV + BishopV + KnightV + PawnV.

chess_num(Values, Coords) :-
    % Definir Coords pecas
    % TODO Subst por maplist
    init_coord(King), init_coord(Queen), init_coord(Rook),
    init_coord(Bishop), init_coord(Knight), init_coord(Pawn),
    Coords = [King, Queen, Rook, Bishop, Knight, Pawn],
    posList(Coords, PosL1),

    valuesCoords(Values, CValues), % get values coords
    posList(CValues, PosL2),
    append(PosL1, PosL2, NPosL),
    all_distinct(NPosL),

    % Valores
    mapValues(Coords, Values),

    flattenList(Coords, L),
    labeling([], L).

wr(L):-
    format('Ki: ~w\tQ: ~w\tR: ~w\tB: ~w\tKn: ~w \tP: ~w \n', L).

test :-
  findall(C, (chess_num([[1, 0]-1, [3, 0]-6, [4, 2]-2, [3, 4]-0], C), wr(C)), _).
