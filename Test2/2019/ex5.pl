% constroi_binarias ve para cada pessoa, [I, K], quais presentes tiveram
% da lista de presentes, Vars.

:-use_module(library(clpfd)).

constroi_bins(_, [], []).
constroi_bins(I, [Gift|GiftList], [LBinH|LBinT]) :-
  I #= H <=> LBinH,
  constroi_bins(I, Gist, LBinT).
