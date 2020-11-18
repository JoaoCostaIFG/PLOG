parseValueList(VL0, VL1, V1P, V2P, Winner) :-
    parseValueListN(VL0, VL1, Winner, V1P, Dif),
    V2P is V1P + Dif.
parseValueListN([], [], 2, 0, 0). % Tie
parseValueListN([V0 | VL0], [V1 | VL1], 0, V0, Dif) :- % Player 0 Wins
    V0 > V1,
    Dif is V1 - V0.
parseValueListN([V0 | VL0], [V1 | VL1], 1, V0, Dif) :- % Player 1 Wins
    V1 > V0,
    Dif is V1 - V0.
parseValueListN([V | VL0], [V | VL1], Winner, NAcc, Dif) :- 
    parseValueListN(VL0, VL1, Winner, Acc, Dif),
    NAcc is Acc + V.
