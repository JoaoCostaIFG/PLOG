:- use_module(library(clpfd)).
:- use_module(library(lists)).

aux(ObjInd, _, _, _, _, 0, []) :-
  ObjInd >= 4.
aux(ObjInd, NEmb, Orcamento, EmbPorObjeto, CustoPorObjeto, EmbUsadas, [I|Objetos]) :-
  ObjInd < 4,
  element(I, EmbPorObjeto, EmbObj),
  element(I, CustoPorObjeto, CustoObj),
  NEmb #>= EmbObj, Orcamento #>= CustoObj,
  %
  NEmb1 #= NEmb - EmbObj,
  Orcamento1 #= Orcamento - CustoObj,
  %
  EmbUsadas #= EmbObj + EmbUsadas1,
  ObjInd1 is ObjInd + 1,
  aux(ObjInd1, NEmb1, Orcamento1, EmbPorObjeto, CustoPorObjeto, EmbUsadas1, Objetos).

force_sort([]).
force_sort([_]).
force_sort([A, B|L]) :-
  A #< B,
  force_sort([B|L]).

constroi(NEmb, Orcamento, EmbPorObjeto, CustoPorObjeto, EmbUsadas, Objetos) :-
  length(Objetos, 4),
  force_sort(Objetos),
  all_distinct(Objetos),
  aux(0, NEmb, Orcamento, EmbPorObjeto, CustoPorObjeto, EmbUsadas, Objetos),
  maximize(labeling([], Objetos), EmbUsadas).
