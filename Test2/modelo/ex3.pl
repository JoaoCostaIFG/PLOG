:- use_module(library(clpfd)).
:- use_module(library(lists)).

p3(L1,L2) :-
    length(L1,N),
    length(L2,N),
    %
    pos(L1,L2),
    test(L2),
    %
    labeling([],L2).

pos([], []).
pos(L1, [X|L2]) :-
    select(X, L1, L3),
    pos(L3, L2).

test([_,_]).
test([X1,X2,X3|Xs]) :-
    (X1 #< X2 #/\ X2 #> X3) #\/ (X1 #> X2 #/\ X2 #< X3),
    test([X2,X3|Xs]).

