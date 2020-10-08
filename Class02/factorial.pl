fact(0, 1). % 0! = 1
fact(Arg, Res) :-
  Arg > 0,
  NewArg is Arg - 1, fact(NewArg, NewRes),
  Res is Arg * NewRes.
