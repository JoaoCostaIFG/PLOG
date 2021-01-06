:-use_module(library(clpfd)).

aux(_, [], [], _, _, []).
aux(MInd, [M|MenHeights_], WomenHeights, WomenLOrig, Delta, [MInd-WInd|Pairs]) :-
  select(W, WomenHeights, WomenHeights_),
  M #>= W,
  M - W #< Delta,
  element(WInd, WomenLOrig, W),
  NextMInd is MInd + 1,
  aux(NextMInd, MenHeights_, WomenHeights_, WomenLOrig, Delta, Pairs).

flatten_pair_list([], []).
flatten_pair_list([P1-P2|Pairs], [P1, P2|FPairs]) :- flatten_pair_list(Pairs, FPairs).

gym_pairs(MenHeights, WomenHeights, Delta, Pairs) :-
  aux(1, MenHeights, WomenHeights, WomenHeights, Delta, Pairs),
  flatten_pair_list(Pairs, FlatPairs),
  labeling([], FlatPairs).
