% recursividade na cauda (economia para a stack de chamadas)

fact(N, F) :- fact(N, 1, F).
fact(0, F, F). % quando chegamos a 0, a answer esta no acumulador

fact(N, Acc, F) :-
  N > 0,
  N1 is N - 1,
  Acc1 is Acc * N,
  fact(N1, Acc1, F).

