% GEN PROBLEM
initNumberedSquares([]).
initNumberedSquares([Coord-Score | NumberedSquares]) :-
    length(Coord, 2),
    domain(Coord, 0, 7),
    % redundant but improves efficiency
    % forbidding 0 on value domain to get more interesting puzzles
    domain([Score], 1, 6),
    initNumberedSquares(NumberedSquares).

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
    permutatedIndsToCoords(PermutatedCoords,
      [King, Queen, Rook, Bishop, Knight, Pawn]).

% restricts all elements in a list to be sorted
forceListSorted([]).
forceListSorted([_ | []]).
forceListSorted([A, B | C]) :-
    A #< B,
    forceListSorted([B | C]).

gen_problem(NCells, [King, Queen, Rook, Bishop, Knight, Pawn],
  NumberedSquares) :-
    NCells < 57, % have to leave 6 free spots for the pieces (63 - 6 = 57)
    Coords = [King, Queen, Rook, Bishop, Knight, Pawn],
    gen_pieceCoords(Coords),

    % a list with the given number (NCells) of NumberedSquares
    length(NumberedSquares, NCells),
    initNumberedSquares(NumberedSquares),

    % for alldistinct
    % strip value of NumberedSquares list
    stripNumberedSquareValue(NumberedSquares, NumberedSquaresCoords),
    % convert coordinates to corresponding integer indexes
    coordToIndexList(Coords, PosL1),
    coordToIndexList(NumberedSquaresCoords, PosL2),
    append(PosL1, PosL2, PosL),
    % all numbered squares and pieces must have distinct coordinates
    all_distinct(PosL),

    % make all NumberedSquares sorted => prevent symmetric results
    % (the order of the numbered squares declaration doesn't affect the result,
    % so different orderings don't result in different problems)
    forceListSorted(PosL2),

    % solve the problem for the given pieces coordinates and numbered squares
    mapValues(Coords, NumberedSquares),

    % flatten the list of the numbered squares coordinates (for labeling)
    flattenList(NumberedSquaresCoords, NumSquaresCoordFlat),
    % labeling and display
    labeling([value(selRandom)], NumSquaresCoordFlat),
    display_board(NumberedSquares),
    display_board(NumberedSquares, Coords).

selRandom(Var, _Rest, BB0, BB1) :-
    fd_set(Var, Set), fdset_to_list(Set, List),
    random_member(Value, List),
    (first_bound(BB0, BB1), Var #= Value ;
     later_bound(BB0, BB1), Var #\= Value).

