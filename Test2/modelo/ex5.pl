:- use_module(library(clpfd)).
:- use_module(library(lists)).

substN(_, _, [], []).
substN(I, H2, [H|T], [H|L]) :-
  I #\= 1,
  I1 #= I - 1,
  substN(I1, H2, T, L).
substN(I, H2, [_H|T], [H2|L]) :-
  I #= 1,
  I1 #= I - 1,
  substN(I1, H2, T, L).

aux(_, [], []).
aux(Rolos, [Presente|Presentes], [RoloInd|RolosSelecionados]) :-
  select(RoloLen, Rolos, _),
  RoloLen #>= Presente,
  element(RoloInd, Rolos, RoloLen),
  %
  RemainingRoloLen #= RoloLen - Presente,
  substN(RoloInd, RemainingRoloLen, Rolos, NewRolos),
  %
  aux(NewRolos, Presentes, RolosSelecionados).

embrulha(Rolos, Presentes, RolosSelecionados) :-
  length(Presentes, NPresentes),
  length(RolosSelecionados, NPresentes),
  %
  aux(Rolos, Presentes, RolosSelecionados),
  %
  labeling([], RolosSelecionados),
  fd_statistics.
