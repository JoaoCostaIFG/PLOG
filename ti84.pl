evaluate(0, 0).
evaluate(s(X), Ret) :-
  evaluate(X, Ret1),
  Ret is Ret1 + 1.
evaluate((Op, T1, T2), Res) :-
  evaluate(T1, V1),
  evaluate(T2, V2),
  ParseOp =.. [Op, V1, V2], Res is ParseOp.
