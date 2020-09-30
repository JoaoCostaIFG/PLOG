pilot('Lamb').
pilot('Besenyei').
pilot('Chambliss').
pilot('MacLean').
pilot('Mangold').
pilot('Jones').
pilot('Bonhomme').

teammember('Breitling', 'Lamb'). % team, pilot
teammember('Red Bull', 'Besenyei').
teammember('Red Bull', 'Chambliss').
teammember('Red Bull', 'Besenyei').
teammember('Mediterranean Racing Team', 'MacLean').
teammember('Cobra', 'Mangold').
teammember('Matador', 'Jones').
teammember('Matador', 'Bonhomme').

plane('MX2', 'Lamb'). % plane, pilot
plane('Edge540', 'Besenyei').
plane('Edge540', 'Chambliss').
plane('Edge540', 'MacLean').
plane('Edge540', 'Mangold').
plane('Edge540', 'Jones').
plane('Edge540', 'Bonhomme').

circuit('Istanbul').
circuit('Budapest').
circuit('Porto').

win('Porto', 'Jones'). % circuit, pilot
win('Budapest', 'Mangold').
win('Istanbul', 'Mangold').

gates('Istanbul', 9). % circuit, gateNum
gates('Budapest', 6).
gates('Porto', 5).

teamwin(T, C) :- win(C, P), teammember(T, P).

% a) win('Porto', X).
% b) teamwin(X, 'Porto').
% c) win(_C1, P), win(_C2, P), _C1 \= _C2.
% d) gates(X, _Y), _Y @> 8.
% e) pilot(P), plane(_P, P), _P \= 'Edge540'.
