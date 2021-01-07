:- use_module(library(clpfd)).
:- use_module(library(lists)).

init_items([], [], []).
init_items([Presente|Presentes], [SelectRolo|RolosSelecionados],
[item(SelectRolo, Presente)|Items]) :-
  init_items(Presentes, RolosSelecionados, Items).

init_bins(_, [], []).
init_bins(I, [Rolo|Rolos], [bin(I, RoloDomain)|Bins]) :-
  RoloDomain in 0..Rolo,
  I1 is I + 1,
  init_bins(I1, Rolos, Bins).

embrulha(Rolos, Presentes, RolosSelecionados) :-
  % domain vars
  length(Presentes, NPresentes),
  length(RolosSelecionados, NPresentes),
  %
  init_items(Presentes, RolosSelecionados, Items),
  init_bins(1, Rolos, Bins),
  %
  bin_packing(Items, Bins),
  %
  labeling([], RolosSelecionados),
  fd_statistics.
