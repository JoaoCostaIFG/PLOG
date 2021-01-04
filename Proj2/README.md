# FEUP PLOG - 3MIEIC03, Group 3 (Chess-Num\_3)

## Authors

João de Jesus Costa - up201806560
João Lucas Silva Martins - up201806436

## Usage instructions

The source code is all inside the **src/** directory.

- Open a sicstus prolog instance
- Consult the `chessnum.pl` file: `consult('chessnum.pl').`
- The predicate `chess_num/2` takes a list of numbered squares as the first
  argument and returns a list of piece coordinates as the second.
- The predicate `chess_num_graphic/1` wraps the `chess_num/2` predicate
  to draw the desired puzzle and solution on screen. The argument is the
  list of numbered squares that represent the problem to solve.
- The predicate `gen_problem/3` takes a quantity of numbered squares as the
  first argument and generates a random puzzle with that quantity of numbered squares.
  The second argument returns the numbered squares and third the coordinates of
  the pieces of the puzzle generated. The puzzle is drawn on screen with and without
  the solution.
- The predicates `test1/1`, `test2/1`, ..., `test10/1`, `test11/1`, run 11 different
  tests, drawing them on-screen.

**Note:** the predicates used for statistics are commented. They are at the end
of the `chess_num/2` predicate and should be uncommented if the user wishes to
see that information.
