:-use_module('info.pl').

% EX1
madeItThrough(Participant) :-
  performance(Participant, Times),
  member(120, Times).

% EX2
juriTimes(Participants, JuriMember, Times, Total) :-
  juriTimesAux(Participants, JuriMember, Times, Total).
  
juriTimesAux([], _, [], 0).
juriTimesAux([P|Participants], JuriMember, [T|Times], Total) :-
  performance(P, Perf),
  mynth1(Perf, JuriMember, T),
  juriTimesAux(Participants, JuriMember, Times, Total1),
  Total is Total1 + T.
  
mynth1([H|_], 1, H).
mynth1([_|T], N, H) :-
  N1 is N - 1,
  mynth1(T, N1, H).
  
% EX3
patientJuri(JuriMember) :-
  performance(Id, Perfs),
  mynth1(Perfs, JuriMember, Val),
  Val = 120,
  performance(Id2, Perfs2),
  Id \= Id2,
  mynth1(Perfs2, JuriMember, Val2),
  Val2 = 120.

% EX4
bestParticipant(P1, P2, P) :-
  performance(P1, Perf1),
  performance(P2, Perf2),
  mysumlist(Perf1, 0, T1),
  mysumlist(Perf2, 0, T2),
  bestParticipantAux(P1, T1, P2, T2, P).

bestParticipantAux(P1, T1, _, T2, P1) :- T1 > T2.
bestParticipantAux(_, T1, P2, T2, P2) :- T1 < T2.

mysumlist([], Ret, Ret).
mysumlist([H|T], Curr, Ret) :-
  Curr1 is Curr + H,
  mysumlist(T, Curr1, Ret).

% EX5
allPerfs :-
  participant(Id, _, Perf),
  performance(Id, Times),
  format('~w:~w:~w', [Id, Perf, Times]), nl,
  fail.
allPerfs.

% EX6
nSuccessfulParticipants(T) :-
  findall(Time, (performance(_, T), mysumlist(T, 0, Time), Time = 480), Times),
  length(Times, T).

% EX7
juriFans(JuriFansList) :-
  findall(Id-ParseTimes, (performance(Id, Times), juriFansAux(Times, ParseTimes, 1)), JuriFansList).

juriFansAux([], [], _).
juriFansAux([T|Other], Other2, Ind) :-
  T \= 120,
  NewInd is Ind + 1,
  juriFansAux(Other, Other2, NewInd).
juriFansAux([T|Other], [Ind|Other2], Ind) :-
  T = 120,
  NewInd is Ind + 1,
  juriFansAux(Other, Other2, NewInd).

% EX8
:- use_module(library(lists)).

eligibleOutcome(Id,Perf,TT) :-
  performance(Id,Times),
  madeItThrough(Id),
  participant(Id,_,Perf),
  sumlist(Times,TT).

nextPhase(N, Participants) :-
  findall(TT-Id-Perf, (participant(Id, _, Perf), eligibleOutcome(Id, Perf, TT)), L),
  sort(L, SortL),
  reverse(SortL, RevL),
  prefix_length(RevL, Participants, N).
  
% EX9
% cut verde - eliminar pesquisa por backtracking à procura de soluções que sabemos não existirem.
filterParticipantsByMaxAge(MaxIdade, [Id|OtherId], [Perf|Perfs]) :-
    participant(Id, Idade, Perf), Idade =< MaxIdade, !,
    predX(MaxIdade, OtherId, Perfs).
filterParticipantsByMaxAge(MaxIdade, [Id|OtherId], Perfs) :-
    participant(Id, Idade, _), Idade > MaxIdade,
    predX(MaxIdade, OtherId, Perfs).
filterParticipantsByMaxAge(_, [], []).

% Dado um número N, pretende-se determinar uma sequência de 2*N números que contenha,
% para todo o k ∈ [1,N], uma sub-sequência Sk = k,…,k começada e terminada com o número k
% e com k outros números de permeio.
% Por exemplo, a sequência [2, 3, 1, 2, 1, 3] cumpre os requisitos: os 1s têm 1 número no meio,
% os 2s têm 2 números no meio, e os 3s têm 3 números no meio. A sequência
% [2, 3, 4, 2, 1, 3, 1, 4] também cumpre. No entanto, alguns valores de N não têm solução possível.
% EX10
impoe(X, L) :-
  length(Mid, X), % Mid é uma lista de tamanho X
  append(L1, [X|_], L), % L1 vai unificar com uma lista de elementos de L até ao primeiro X
  append(_, [X|Mid], L1). % Mid unifica com os elementos de L

% verifica se o primeiro e ultimo elementos da lista são X e estão distanciados X
% elementos

% EX11
langford(N, L) :-
  N2 is N * 2,
  length(L, N2),
  langfordAux(N, L).
langfordAux(0, _).
langfordAux(N, L) :-
  impoe(N, L),
  N1 is N - 1,
  langfordAux(N1, L).
