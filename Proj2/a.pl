:-use_module(library(clpfd)).

b(I, N, [], Osave) :-
  I #=< N,
  I1 #= I + 1,
  b(I1, N, Osave, Osave).
b(I, N, [O | Os], _) :-
  I #=< N,
  O #= I.
b(I, N, [_ | Os], Osave) :-
  I #=< N,
  b(I, N, Os, Osave).

a(L) :-
  L = [A, B, C, D],
  domain(L, 1, 6),
  all_distinct(L),

  A #=< B,
  b(A, B, [C, D], [C, D]),

  labeling([], L).

% others_not_between_loop(_, []).
% others_not_between_loop(BlackListID, [OtherCoord | Others]) :-
    % elemToID(OtherCoord, OtherID),
    % BlackListID #\= OtherID,
    % others_not_between_loop(BlackListID, Others).
% others_not_between([RX, RY]-h-[X, Y], _) :- RX #>= X.
% others_not_between([RX, RY]-h-[X, Y], Others) :-
    % RX #< X,
    % elemToID([RX, RY], BlackListID),
    % others_not_between_loop(BlackListID, Others),
    % RX1 #= RX + 1,
    % others_not_between([RX1, RY]-h-[X, Y], Others).

% others_is_between([RX, RY]-h-[X, Y], [], O) :-
    % RX #< X,
    % RX1 #= RX + 1,
    % others_is_between([RX1, RY]-h-[X, Y], O, O).
% others_is_between([RX, RY]-h-[X, Y], [OtherCoord | Others], O) :-
    % RX #< X,
    % elemToID([RX, RY], BlackListID),
    % elemToID(OtherCoord, OtherID),
    % BlackListID #= OtherID.
% others_is_between([RX, RY]-h-[X, Y], [_ | Others], O).
    % RX #< X,
    % others_is_between([RX, RY]-h-[X, Y], Others, O).

% % ROOK
% % if on the same line, attacks is has line of sight (horizontal)
% valueRook([RX, RY], [X, Y], 1, Others) :-
    % RY #= Y,
    % bigger(RX, X, BigX, SmallX),
    % others_not_between([SmallX, Y]-h-[BigX, Y], Others).
% valueRook([RX, RY], [X, Y], 0, Others) :-
    % RY #= Y,
    % bigger(RX, X, BigX, SmallX),
    % others_is_between([SmallX, Y]-h-[BigX, Y], Others, Others).
% % % if on the same column, attacks is has line of sight (vertical)
% valueRook([RX, RY], [X, Y], 1, Others) :-
    % RX #= X,
    % bigger(RY, Y, BigY, SmallY),
    % others_not_between([RX, SmallY]-v-[X, BigY], Others).
% valueRook([RX, RY], [X, Y], 0, Others) :-
    % RX #= X,
    % bigger(RY, Y, BigY, SmallY),
    % others_is_between([RX, SmallY]-v-[X, BigY], Others, Others).
% % % if not on the same line and column, doesn't attack
% valueRook([RX, RY], [X, Y], 0, _) :-
    % RX #\= X, RY #\= Y.
