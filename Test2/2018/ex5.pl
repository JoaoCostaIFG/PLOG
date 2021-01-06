:-use_module(library(clpfd)).
:-use_module(library(lists)).

aux([], _, _, _, _, []). % no more men
aux(_, _, [], _, _, []). % no more women
aux(MenHeights, MenLOrig, WomenHeights, WomenLOrig, Delta, [MInd-WInd|Pairs]) :-
  select(M, MenHeights, MenHeights_),
  select(W, WomenHeights, WomenHeights_),
  M #>= W,
  M - W #< Delta,
  element(MInd, MenLOrig, M),
  element(WInd, WomenLOrig, W),
  aux(MenHeights_, MenLOrig, WomenHeights_, WomenLOrig, Delta, Pairs).
aux(_, _, _, _, _, []). % end but still has pairs

flatten_pair_list([], []).
flatten_pair_list([P1-P2|Pairs], [P1, P2|FPairs]) :- flatten_pair_list(Pairs, FPairs).

force_sort([]).
force_sort([_]).
force_sort([A-_B, C-_D|L]) :-
  A #< C,
  force_sort([C-_D|L]).

optimal_skating_pairs(MenHeights, WomenHeights, Delta, Pairs) :-
  aux(MenHeights, MenHeights, WomenHeights, WomenHeights, Delta, Pairs),
  force_sort(Pairs),
  flatten_pair_list(Pairs, FlatPairs),
  length(Pairs, NumPairs),
  maximize(labeling([], FlatPairs), NumPairs).

