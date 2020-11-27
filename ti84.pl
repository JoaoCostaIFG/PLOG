evaluate(0, 0).
evaluate(s(X), Ret) :-
  evaluate(X, Ret1),
  Ret is Ret1 + 1.
evaluate((Op, T1, T2), Res) :-
  evaluate(T1, V1),
  evaluate(T2, V2),
  ParseOp =.. [Op, V1, V2], Res is ParseOp.

z(0,0). z(s(A),C+1):-z(A,C). z((O,A,B),C):-z(A,D),z(B,E),P=..[O,D,E],(C)is(P).

theprologs(Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjf,Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjy):-(Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjf)is(((44717//723)*((345+378)//(14+3)*17+9)+614)/\20818),(Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjy)is(0). theprologs(s(Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjk),Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjm+1):-theprologs(Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjk,Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjm). theprologs((Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodji,Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjk,Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjl),Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjm):-theprologs(Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjk,Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjq),theprologs(Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjl,Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodja),Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjc=..[Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodji,Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjq,Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodja],(Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjm)is(Zpasokdopkasopkdopasdjasdpasdjapsdkasdpdodjc).
