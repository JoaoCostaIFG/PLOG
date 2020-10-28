:- use_module(library(lists)).

ligado(a,b).
ligado(a,c). 
ligado(f,j). 
ligado(b,d). 
ligado(f,k). 
ligado(b,e). 
ligado(g,l). 
ligado(b,f). 
ligado(g,m). 
ligado(c,g). 
ligado(k,n). 
ligado(d,h). 
ligado(l,o). 
ligado(d,i). 
ligado(i,f). 

% a) depth first

depth_path(A, B, Solution) :-
  depth_first([], A, B, Solution_inv),
  reverse(Solution_inv, Solution).

depth_first(Path, B, B, [B|Path]).
depth_first(Path, A, B, Solution_inv) :-
  ligado(A, A1),
  \+member(A1, Path),
  depth_first([A|Path], A1, B, Solution_inv).

% b) breath first

breath_path(A, B, Solution) :-
  breath_first([[]|_], A, B, Solution_inv, []),
  reverse(Solution_inv, Solution).

breath_first([[]|Path], B, B, [B|Path], Process).

breath_first([[HPath]|Path], A, B, Solution_inv, []) :-
  bagof(A1, ligado(A, A1), L),
  breath_first([HPath]|Path], A, B, Solution_inv, L).

breath_first([[HPath]|Path], A, B, Solution_inv, [P1|Process]) :-
  \+member(A, HPath),
  breath_first([[A|HPath]|Path], P1, B, Solution_inv, Process).
