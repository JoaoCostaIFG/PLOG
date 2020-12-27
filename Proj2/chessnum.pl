:-use_module(library(clpfd)).

init(L) :-
    L = [
        [0, 1, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0]
    ].


chess-num :-
    init(L),
    write(L).

    %Cols=[A1,A2,A3,A4],
    %domain(Cols,1,4),
    %all_distinct(Cols),
    %labeling([],Cols).
