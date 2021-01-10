:-use_module(library(clpfd)).

aaa(StartTimes) :-
  Tasks = [
    task(S1, 5, E1, [2, 1], 1),
    task(S2, 5, E2, [4, 1], 2),
    task(S3, 5, E3, [3, 1], 3)
  ],
  Capacities = [
    cumulative(10),
    cumulative(2)
  ],
  %
  StartTimes = [S1, S2, S3],
  domain(StartTimes, 0, 6),
  EndTimes = [E1, E2, E3],
  maximum(TimeTaken, EndTimes),
  %
  multi_cumulative(Tasks, Capacities),
  labeling([], StartTimes),
  format('~w\n~w\n', [StartTimes, EndTimes]).
