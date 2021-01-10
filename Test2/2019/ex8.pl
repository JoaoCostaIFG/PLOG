:-use_module(library(clpfd)).

objeto(piano, 3, 30).
objeto(cadeira, 1, 10).
objeto(cama, 3, 15).
objeto(mesa, 2, 15).
homens(4).
tempo_max(60).

make_tasks(_, [], [], [], []).
make_tasks(I, [_-StartTime-Duration-EndTime-Resources-I|Objs],
 [task(StartTime, Duration, EndTime, Resources, I)|Tasks], [StartTime|StartTimes], [EndTime|EndTimes]) :-
  I1 is I + 1,
  make_tasks(I1, Objs, Tasks, StartTimes, EndTimes).

print_result([]).
print_result([Name-StartTime-Duration-EndTime-_-_|Objs]) :-
  format('~w: ~w-~w (~w)', [Name, StartTime, EndTime, Duration]),
  nl,
  print_result(Objs).

furniture :-
  homens(NumMen),
  tempo_max(MaxTime),
  %
  findall(Name-_StartTime-Duration-_EndTime-Resources-_I,
   objeto(Name, Resources, Duration), Objetos),
  make_tasks(1, Objetos, Tasks, Ss, Es),
  domain(Ss, 0, MaxTime),
  %
  maximum(TimeTaken, Es),
  TimeTaken #=< MaxTime, % cant go over MaxTime
  %
  cumulative(Tasks, [limit(NumMen)]),
  labeling([minimize(TimeTaken)], Ss),
  format('Time taken: ~w', [TimeTaken]), nl,
  print_result(Objetos).

