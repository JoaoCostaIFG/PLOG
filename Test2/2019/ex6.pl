:-use_module(library(clpfd)).
:-use_module(library(lists)).

flatten2([], []) :- !.
flatten2([L|Ls], FlatL) :-
    !,
    flatten2(L, NewL),
    flatten2(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten2(L, [L]).

armario([[20, 30, 6, 50], [50, 75, 15, 125], [60, 90, 18, 150], [30, 45, 9, 75], [40, 60, 12, 100]]).
objetos([113-54, 305-30, 295-53, 376-39, 468-84, 114-48, 337-11, 259-80, 473-28, 386-55, 258-39, 391-37, 365-76, 251-18, 144-42, 399-94, 463-48, 473-9, 132-56, 367-8]).

init_items([], [], []).
init_items([_-ObjetoVol|Objetos], [Var|Vars], [item(Var, ObjetoVol)|Items]) :-
  init_items(Objetos, Vars, Items).

init_bins(_, [], []).
init_bins(I, [SpotVol|Spots], [bin(I, SpotDomain)|Bins]) :-
  SpotDomain in 0..SpotVol,
  I1 is I + 1,
  init_bins(I1, Spots, Bins).

weight_in_n(_, [], [], 0).
weight_in_n(N, [Weight-_|Objetos], [Var|Vars], Ret) :-
  Var #= N #<=> IsInN,
  Ret #= (IsInN * Weight) + Ret1,
  weight_in_n(N, Objetos, Vars, Ret1).

weight_in_prateleiras(I, NSpots, _, _, _) :-
  I > NSpots.
weight_in_prateleiras(I, NSpots, Objetos, Vars, [SpotWeight|WeightInPrateleiras]) :-
  I =< NSpots,
  weight_in_n(I, Objetos, Vars, SpotWeight),
  I1 is I + 1,
  weight_in_prateleiras(I1, NSpots, Objetos, Vars, WeightInPrateleiras).

sort_col(I, N, _, _) :-
  I >= N.
sort_col(I, N, Step, WeightInPrateleiras) :-
  I < N, % don't put = here
  I1 is I + Step,
  element(I, WeightInPrateleiras, Elem1),
  element(I1, WeightInPrateleiras, Elem2),
  Elem1 #=< Elem2,
  sort_col(I1, N, Step, WeightInPrateleiras).
sort_weights(I, _, NumCols, _) :-
  I > NumCols.
sort_weights(I, NumNiveis, NumCols, WeightInPrateleiras) :-
  I =< NumCols,
  N is I + ((NumNiveis - 1) * NumCols), % last elem of col Index
  sort_col(I, N, NumCols, WeightInPrateleiras),
  I1 is I + 1,
  sort_weights(I1, NumNiveis, NumCols, WeightInPrateleiras).

prat(Prateleiras, Objetos, Vars) :-
  length(Prateleiras, NumNiveis),
  nth0(0, Prateleiras, P1),
  length(P1, NumCols),
  %
  length(Objetos, NumObjetos),
  length(Vars, NumObjetos),
  % limit volume
  init_items(Objetos, Vars, VolItems),
  flatten2(Prateleiras, FlatPrateleiras),
  init_bins(1, FlatPrateleiras, VolBins),
  bin_packing(VolItems, VolBins),
  % sorted weight
  length(FlatPrateleiras, NumSpots),
  length(WeightInPrateleiras, NumSpots),
  weight_in_prateleiras(1, NumSpots, Objetos, Vars, WeightInPrateleiras),
  sort_weights(1, NumNiveis, NumCols, WeightInPrateleiras),
  %
  labeling([], Vars),
  fd_statistics.

big_example(Vars) :-
  armario(A), objetos(Objs), prat(A, Objs, Vars).
small_example(Vars) :-
  prat([[30, 6], [75, 15]], [176-40, 396-24, 474-35, 250-8, 149-5, 479-5], Vars).

