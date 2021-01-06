:-dynamic entrouAux/1.

:-use_module('info.pl').

% EXa
filme_de(Sequela, Subtitulo, Ano) :-
  sequela(Sequela, FilmeId, _),
  filme(FilmeId, Subtitulo, Ano, _).

% EXb
entrou_na_sequela(Ator, Sequela) :-
  retractall(entrouAux(_)),
  assert(entrouAux([])),

  sequela(Sequela, Id, _),
  filme(Id, _, _, Atores),
  member(_-Ator, Atores),
  entrouAux(L),
  \+member(Id, L),
  retract(entrouAux(_)),
  assert(entrouAux([Id|L])).

% EXc
atores(Subtitulo, ListaAtores) :-
  findall(Atores, (filme(_, Subtitulo, _, L), member(_-Atores, L)), ListaAtores).

% EXd
manteve_ator(Sequela, Personagem) :-
  sequela(Sequela, Id, _),
  findall(Ator, (filme(Id, _, _, L), member(Personagem-Ator, L)), Atores),
  manteve_atorAux(Atores).

manteve_atorAux([A,B|Tores]) :-
  A = B,
  manteve_atorAux([B|Tores]).
manteve_atorAux([]).
manteve_atorAux([_]).

% EX2-a
%findall -> len

% EX2-b
:-use_module(library(lists)).
imigrou(1, data(2000, 02, 04)).
imigrou(3, data(1000, 09, 14)).
imigrou(2, data(2000, 09, 14)).
imigrou(22, data(50, 09, 14)).

imigrante_mais_recente(IdPessoa) :-
  findall(Date-Id, (imigrou(Id, data(Y, M, D)), Date is Y*10000+M*100+D), Imigrantes),
  sort(Imigrantes, ImigrantesSort),
  reverse(ImigrantesSort, [_-IdPessoa|_]).
