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

mapValues(_, []).
mapValues(Coords, [Coord-V | L1]) :-
    value(Coords, Coord, V),
    mapValues(Coords, L1).

abs_between(Lower, Upper, L) :-
    Upper #> Lower,
    NLower #= Lower + 1,
    NUpper #= Upper - 1,
    bagof(N, between(NLower, NUpper, N), L).
abs_between(Upper, Lower, L) :-
    Upper #> Lower,
    NLower #= Lower + 1,
    NUpper #= Upper - 1,
    bagof(N, between(NLower, NUpper, N), L).

% VALUES

valueKing([KX, KY], [X, Y], V) :-
    DiffX #= KX - X,
    DiffY #= KY - Y,
    (abs(DiffX) #< 2 #/\ abs(DiffY) #<2) #<=> V.

valueQueen([QX, QY], [X, Y], V) :-
    ((abs(QY - Y) #= abs(QX - X)) #\/ (QX #= X) #\/ (QY #= Y)) #<=> V.

rookBlackList([RX, RY], [X, Y]) :-
    RX #= X,
    abs_between(RY, Y, L).
rookBlackList([RX, RY], [X, Y]) :-
    RY #= Y,
    abs_between(RX, X, L).

valueRook([RX, RY], [X, Y], 1) :- 
    rookBlackList([RX, RY], [X, Y]).
valueRook(_, _, 0).

valueBishop([BX, BY], [X, Y], V) :-
    (abs(BY - Y) #= abs(BX - X)) #<=> V.

valueKnight([KX, KY], [X, Y], V) :-
    ((abs(KX - X) #= 1 #/\ abs(KY - Y) #= 2) #\/
        (abs(KX - X) #= 2 #/\ abs(KY - Y) #= 1)) #<=> V.

valuePawn([PX, PY], [X, Y], V) :-
    (PY - Y #= 1 #/\ abs(PX - X) #= 1) #<=> V.

value([King, Queen, Rook, Bishop, Knight, Pawn], Coord, V) :-
    valueKing(King, Coord, KingV),
    valueQueen(Queen, Coord, QueenV),
    valueRook(Rook, Coord, RookV),
    valueBishop(Bishop, Coord, BishopV),
    valueKnight(Knight, Coord, KnightV),
    valuePawn(Pawn, Coord, PawnV),
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
