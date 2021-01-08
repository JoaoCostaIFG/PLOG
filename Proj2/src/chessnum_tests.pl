% TESTS
% The tests were taken from:
% https://erich-friedman.github.io/puzzle/chessnum/
% They can also be found in the article
test1(C) :-
    % sol: [[3, 1], [6, 0], [2, 0], [0, 3], [1, 1], [4, 1]]
    chess_num_graphic([[1, 0]-1, [3, 0]-6, [4, 2]-2, [3, 4]-0], C).

test2(C) :-
    chess_num_graphic([[2, 7]-4, [2, 1]-4, [6, 3]-4, [0, 5]-0], C).

test3(C) :-
    chess_num_graphic([[7, 3]-0, [7, 4]-0, [6, 4]-0, [1, 0]-0, [0, 0]-1, [5, 7]-0, [5, 6]-0,
      [3, 3]-0, [4, 3]-0, [0, 2]-0, [5, 0]-0, [7, 7]-1, [7, 0]-1], C).

test4(C) :-
    chess_num_graphic([[4, 4]-5, [2, 1]-3, [3, 6]-3, [7, 6]-0, [7, 1]-0], C).

test5(C) :-
    chess_num_graphic([[5, 4]-4, [1, 0]-0, [2, 0]-0, [5, 0]-0, [7, 1]-0, [7, 2]-0, [0, 4]-0,
      [7, 4]-0, [7, 6]-0, [2, 7]-0, [5, 7]-0, [6, 7]-0, [7, 7]-0], C).

test6(C) :-
    chess_num_graphic([[0, 0]-0, [7, 0]-0, [0, 6]-0, [0, 7]-0, [6, 7]-0,
      [2, 2]-1, [3, 2]-1, [4, 2]-1, [5, 2]-1,
      [2, 3]-1, [3, 3]-1, [4, 3]-1, [5, 3]-1,
      [2, 4]-1, [3, 4]-1, [4, 4]-1, [5, 4]-1,
      [2, 5]-1, [3, 5]-1, [4, 5]-1, [5, 5]-1], C).

test7(C) :-
    % chess_num_graphic([[0, 1]-1, [1, 1]-1, [2, 1]-1, [3, 1]-1, [4, 1]-1, [5, 1]-1, [6, 1]-1,
      % [7, 1]-1, [0, 6]-1, [1, 6]-1, [2, 6]-1, [5, 6]-1, [6, 6]-1, [6, 7]-1,
      % [7, 6]-2, [5, 7]-2, [4, 6]-2, [3, 6]-2], C).
    chess_num_graphic([[7, 6]-2, [5, 7]-2, [4, 6]-2, [3, 6]-2, [0, 1]-1, [1, 1]-1, [2, 1]-1,
      [3, 1]-1, [4, 1]-1, [5, 1]-1, [6, 1]-1, [7, 1]-1, [0, 6]-1, [1, 6]-1,
      [2, 6]-1, [5, 6]-1, [6, 6]-1, [6, 7]-1], C).

test8(C) :-
    chess_num_graphic([[0, 0]-0, [0, 1]-0, [1, 1]-0, [1, 2]-0, [2, 2]-0, [2, 3]-0, [3, 3]-0,
      [3, 4]-0, [4, 4]-0, [4, 5]-0, [5, 5]-0, [5, 6]-0, [6, 6]-0, [6, 7]-0,
      [7, 7]-0, [2, 6]-2], C).

test9(C) :-
    chess_num_graphic([[2, 4]-4, [4, 4]-4, [2, 3]-3, [4, 3]-3, [2, 0]-0, [3, 0]-0, [6, 7]-0,
      [2, 2]-2, [4, 2]-2, [2, 1]-1, [4, 1]-1], C).

test10(C) :-
    chess_num_graphic([[4, 7]-3, [0, 0]-0, [1, 0]-0, [2, 0]-0, [3, 0]-0, [4, 0]-0, [5, 0]-0,
      [6, 0]-0, [7, 0]-0, [2, 6]-2, [0, 1]-1, [1, 1]-1, [2, 1]-1, [3, 1]-1,
      [4, 1]-1, [5, 1]-1, [6, 1]-1, [7, 1]-1, [0, 5]-1], C).

test11(C) :-
    chess_num_graphic([[6, 1]-3, [1, 4]-3, [6, 4]-3, [4, 6]-3, [2, 1]-2, [3, 2]-2, [2, 7]-1], C).